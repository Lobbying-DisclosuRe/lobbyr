####### Load necessary libraries for the app's functionality
library(shiny)
library(lobbyR)
library(bslib)
library(DT)
library(keyring)
library(shinyjs)

####### Helper function to build a string representing the get_filings() call with the user's parameters
build_call_string <- function(params) {
  format_value <- function(x) {
    if (length(x) == 0) return("NULL")
    if (is.character(x) && all(x == "")) return("NULL")
    if (is.null(x)) return("NULL")

    if (is.character(x) && length(x) > 1) {
      return(paste0('c("', paste(x, collapse = '", "'), '")'))
    }
    if (is.character(x)) return(paste0('"', x, '"'))
    if (is.logical(x)) return(ifelse(x, "T", "F"))
    x
  }

  sprintf(
    'get_filings(\n  issues = %s,\n  issue_joiner = %s,\n  year = %s,\n  filing_period = %s,\n  client_name = %s,\n  registrant_name = %s,\n  ending_date = %s,\n  starting_date = %s,\n  tidy_result = %s,\n  ignore_disclaimer = %s\n)',
    format_value(params$issues),
    format_value(params$issue_joiner),
    format_value(params$year),
    format_value(params$filing_period),
    format_value(params$client_name),
    format_value(params$registrant_name),
    format_value(params$ending_date),
    format_value(params$starting_date),
    format_value(params$tidy_result),
    format_value(params$ignore_disclaimer)
  )
}

####### Define the user interface (UI) for the app
ui <- page_fluid(
  tags$head(
    tags$style(HTML("
    /* === General App Layout === */
    p {
      font-size: 0.8em;
    }
    .label {
      font-size: 1em;
    }
    #main_status {
      width: 100%;
      white-space: pre-wrap;
      word-break: break-word;
      overflow-x: hidden;
    }

    /* === Action Buttons === */
    #actionbuttons {
      padding: 0.2em 0 0.2em 0;
    }
    button.action-button {
      margin: 0.2em;
    }

    /* === Cleaning Helpers Box === */
    #cleaning_helpers {
      border: 2px solid red;
      padding: 10px;
      border-radius: 5px;
    }

    /* === Disclaimer Section === */
    #disclaimer {
      font-size: 0.8em;
    }

    /* === DataTable Styling === */
    /* Container for DTOutput(data_table) */
    #data_table {
      height: 500px;           /* Set table height */
      overflow-y: auto;        /* Scroll table as a whole (header + body) */
    }
    #data_table table.dataTable {
      margin: 0 !important;
    }
    /* Table cell formatting */
    table.dataTable th,
    table.dataTable td {
      max-width: 225px;
      overflow: hidden;
      white-space: nowrap;
      word-wrap: break-word;
    }

    /* === Remove Independent Header/Body Scroll === */
    /* Remove DataTables scrollBody/scrollHead custom scrollbars */
    /* (scrollY is not set in datatable options, so these are just for fallback) */
    .dataTables_wrapper .dataTables_scrollBody {
      overflow-x: scroll !important;
      overflow-y: scroll !important;
    }
    .dataTables_scrollHead {
      overflow-x: scroll !important;
    }
  "))
  ),#end head tags
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(version = 5,
                          bootswatch = "journal",
                          bg = "#0b3d91", fg = "white", primary = "#FCC780",
                          base_font = font_collection(font_google("Roboto", local = FALSE), "Pacifico", "sans-serif")
  ),
  titlePanel("lobbyR: Making it easier to get federal lobbying disclosures"),
  sidebarLayout(
    sidebarPanel(
      actionLink("toggleMath", "Quick math"),
      shinyjs::hidden(
        div(id = "math_dropdown",
            p("This gives a quick ballpark on how much income or expenses an entity might have spent on lobbying."),
            p("DISCLAIMER: Use checkers and inspect data to make sure the number is accurate."),
            actionButton("sum_totals", "Sum totals", class = "btn-primary")
        ) #end math dropdown
      ), #end hidden math box
      div(id = "myapp",
          textInput("issues", "Issues (comma-separated)", value = ""),
          radioButtons("issue_joiner", "Issue Joiner:",
                       choices = c("and", "or"),
                       selected = "",
                       inline = TRUE),
          selectInput("year", "Year",
                      choices = c("", 2025:1998),
                      selected = "2025"),
          textInput("client_name", "Client Name (one client only)", value = ""),
          checkboxInput("ignore_disclaimer", "Ignore Disclaimer", value = FALSE),
          checkboxInput("tidy_result", "Avoid raw data", value = TRUE),
          div(actionLink("toggleAdvanced", "Show/Hide search fields"), style = "margin-bottom: 0.5em;"), #separate_lines
          div(actionLink("toggleAPI", "Set/Check API Key"), style = "margin-bottom: 0.5em;"), #separate_lines
          shinyjs::hidden(
            div(id = "advanced",
                textInput("registrant_name", "Registrant Name (one registrant only)", value = ""),
                selectInput("filing_period", "Filing Period",
                            choices = c("", "first_quarter", "second_quarter", "third_quarter", "fourth_quarter")),
                dateInput("starting_date", "Starting Date", value = "1998-01-01", format = "yyyy-mm-dd"),
                dateInput("ending_date", "Ending Date", value = NULL, format = "yyyy-mm-dd"),
                textInput("min_amount", "Minimum Amount (USD)", value = ""), #min/max_boxes
                textInput("max_amount", "Maximum Amount (USD)", value = ""), #min/max_boxes
                width = 12
            ) #end div advanced
          ), #end advanced options
          shinyjs::hidden(
            div(
              id = "api_key_section",
              h4("API Key Setup"),
              br(),
              verbatimTextOutput("status"),
              br(),
              passwordInput("api_key_input", "Enter your Senate API key:", ""),
              actionButton("save_key", "Save Key", class = "btn-success"),
              helpText("Note: Keys are stored securely in your system's keychain"),
              br(),
              actionButton("verify_key", "Verify Key", class = "btn-info"),
              br(), br(),
              actionButton("delete_key", "Delete Key", class = "btn-danger"),
              br(), br(),
              helpText("API keys can be requested at:"),
              tags$a(href = "https://lda.senate.gov/api/register/",
                     target = "_blank",
                     "https://lda.senate.gov/api/register/")
            )
          ), #end api helper
          div(id = "actionbuttons",
              actionButton("query", "Query Filings"),
              actionButton("reset", "Reset Form")
          ), #end action buttons
          div( id = "cleaning_helpers",
               h5("Data cleaning options:"),
               h6("Use these once you've made your query to try and clean the data"),
               br(), # <-- Add line break after conflict section
               actionLink("toggledupes", "Show dupe checker"),
               shinyjs::hidden(
                 div(id = "dupes",
                     radioButtons(
                       "dupes_group",
                       "Checkbox group",
                       choices = c(
                         "Flag duplicates" = "a",
                         "Attempt to clean" = "b",
                         "Revert to original data" = "c"
                       ),
                       selected = character(0)  # Start with nothing selected
                     ) #end checkbox group input
                 ) #end dupes div
               ), #end hidden box
               br(), # <-- Add line break after conflict section
               actionLink("toggleconflict", "Show conflict checker"),
               shinyjs::hidden(
                 div(id = "conflict",
                     radioButtons(
                       "conflict_group",
                       "Check for Conflict",
                       choices = c(
                         "Flag conflicts" = "a",
                         "Attempt to clean" = "b",
                         "Revert to original data" = "c"
                       ),
                       selected = character(0)  # Start with nothing selected
                     ) #end conclict checkbox input
                 ) #end dupes div
               ), #end hidden box
               br(), # <-- Add line break after conflict section
               br(), # <-- Add line break after conflict section
               br(), # <-- Add line break after conflict section

               div( id = "disclaimer",
                    p("Make sure to read the documentation for",tags$a(href = "https://github.com/Lobbying-DisclosuRe/lobbyr?tab=readme-ov-file#flag_dupes", "flag dupes"), "and", tags$a(href = "https://github.com/Lobbying-DisclosuRe/lobbyr/blob/main/README.md#flag_client_registrant_conflict", "flag registrant conflict"), "before using these options to understand better how it may impact your data."), h6("!IMPORTANT: If after using cleaning data functions a user wants to just go back to flagging it, you must select revert to original data before doing so or else you may get an inaccurate count.")
               ),#end disclaimer
          ), #end checkbox div
      ), #end div my app
      width = 3
    ), #end sidebar panel
    ##### Start Main PANEL #######
    mainPanel(
      h4("Interactive DataTable"),
      downloadButton("download_all", "Download All Results"), ##add download all button
      DTOutput("data_table"),  # shows dt table
      h4("Status/Response"),
      verbatimTextOutput("main_status"),
      h5("Query readout"),
      verbatimTextOutput("results"),
      width = 9
    ) #end mainbar panel
  ) #end sidebar layout
) #end ui page fluid

####### Define the server logic for the app
server <- function(input, output, session) {
  ####### Create a reactiveValues object to store the main data frame
  rv <- reactiveValues(
    result_df = NULL,  # Will hold the dataframe for cleaning
    original_df = NULL, ##don't revert to orig data for cleaning
    display_df = NULL   ##don't revert to orig data for cleaning
  )
  status_message <- reactiveVal("") ##passthrough messages

  ####### Observe the Query button and handle API querying and table rendering
  observeEvent(input$query, {
    # ---- Show custom "Iterating" modal dialog ----
    showModal(
      modalDialog(
        title = "Iterating",
        div(
          tags$h4("Querying the Senate LDA database. This could take a while, depending on the number of results."),
          tags$div(class = "spinner-border", role = "status", style = "margin:10px;"),
          "Please wait."
        ),
        size = "l",
        easyClose = FALSE,
        fade = TRUE,
        footer = NULL
      )
    )
    ####### Build the parameter list for the API call based on user inputs
    param_list <- list(
      issues = if (nzchar(input$issues)) unlist(strsplit(input$issues, ",\\s*")) else "",
      issue_joiner = input$issue_joiner,
      year = if (nzchar(input$year)) input$year else "",
      filing_period = if (nzchar(input$filing_period)) input$filing_period else "",
      client_name = if (nzchar(input$client_name)) input$client_name else "",
      registrant_name = if (nzchar(input$registrant_name)) input$registrant_name else "",
      starting_date = if (!is.null(input$starting_date)) format(input$starting_date, "%Y-%m-%d") else "",
      ending_date = if (!is.null(input$ending_date)) format(input$ending_date, "%Y-%m-%d") else "",
      tidy_result = input$tidy_result,
      ignore_disclaimer = input$ignore_disclaimer,
      min_amount = input$min_amount, #min/max_boxes
      max_amount = input$max_amount  #min/max_boxes
    )

    ####### Remove empty parameters from the list
    param_list <- param_list[!sapply(param_list, function(x) {
      is.null(x) || (length(x) > 0 && all(x == ""))
    })]

    ####### Show the constructed API call string in the UI
    output$results <- renderPrint({
      cat(build_call_string(param_list))
    })

    ####### Call the API and handle messages/errors for status output
    result <- withCallingHandlers( ##passthrough messages
      tryCatch({ ##passthrough messages
        api_result <- do.call(get_filings, param_list) ##passthrough messages
        api_result ##passthrough messages
      }, error = function(e) { ##passthrough messages
        return(paste("API Error:", conditionMessage(e))) ##passthrough messages
      }), ##passthrough messages
      message = function(m) { ##passthrough messages
        status_message(m$message) ##passthrough messages
      } ##passthrough messages
    ) ##passthrough messages

    # ---- Remove the modal after processing is done ----
    removeModal()

    ####### Update the status message output
    output$main_status <- renderText({ ##passthrough messages
      status_message() ##passthrough messages
    }) ##passthrough messages

    ####### Store the result in the reactiveValues object for later use/cleaning
    if (is.character(result)) {
      rv$result_df <- NULL  # Clear on error
      rv$original_df <- NULL ##don't revert to orig data for cleaning
      rv$display_df <- NULL  ##don't revert to orig data for cleaning
    } else {
      rv$result_df <- result  # Store for cleaning
      rv$original_df <- result ##don't revert to orig data for cleaning
      rv$display_df <- result  ##don't revert to orig data for cleaning
    }

    ####### Render the DataTable with renamed columns, hyperlinks, and currency formatting
    output$data_table <- renderDataTable({
      df <- rv$display_df ##don't revert to orig data for cleaning
      if (is.null(df)) {
        datatable(data.frame(Error = "No data"), rownames = FALSE) ##don't revert to orig data for cleaning
      } else {
        col_rename <- c(
          "registrant.name" = "registrant",
          "client.name" = "client",
          "filing_type_display" = "filing type",
          "dt_posted" = "date posted",
          "filing_document_url" = "filing url",
          "registrant.description" = "registrant description",
          "client.general_description" = "client description",
          "filing_type" = "filing code",
          "filing_period" = "filing period"
        )
        df <- as.data.frame(df) ##don't revert to orig data for cleaning
        old_names <- names(df) ##don't revert to orig data for cleaning
        names(df) <- ifelse(old_names %in% names(col_rename), col_rename[old_names], old_names) ##don't revert to orig data for cleaning
        if ("filing url" %in% names(df)) {
          df[["filing url"]] <- ifelse(
            is.na(df[["filing url"]]) | df[["filing url"]] == "",
            "",
            sprintf('<a href="%s" target="_blank">%s</a>', df[["filing url"]], df[["filing url"]])
          )
        }
        if ("income" %in% names(df)) df$income <- as.numeric(gsub("[^0-9.-]", "", df$income))
        if ("expenses" %in% names(df)) df$expenses <- as.numeric(gsub("[^0-9.-]", "", df$expenses))
        dt <- datatable(
          df,
          options = list(
            pageLength = -1,
            scrollX = TRUE,
            scrollY = "600px",
            dom = 'Bfrtip'
          ),
          filter = 'top',
          rownames = FALSE,
          escape = FALSE
        )
        if ("income" %in% names(df)) {
          dt <- DT::formatCurrency(dt, "income", currency = "$", digits = 2)
        }
        if ("expenses" %in% names(df)) {
          dt <- DT::formatCurrency(dt, "expenses", currency = "$", digits = 2)
        }
        dt
      }
    })
  }) #end observe event for api query

  ##### Flag dupes radio buttons cleaning functionality
  observeEvent(input$dupes_group, {
    req(rv$display_df) ##don't revert to orig data for cleaning
    if(input$dupes_group == "a") {
      cleaned_data <- flag_dupes(rv$display_df, find_duplicates = TRUE, attempt_cleaning = FALSE) ##don't revert to orig data for cleaning
      rv$display_df <- cleaned_data ##don't revert to orig data for cleaning
    } else if(input$dupes_group == "b") {
      cleaned_data <- flag_dupes(rv$display_df, find_duplicates = TRUE, attempt_cleaning = TRUE) ##don't revert to orig data for cleaning
      rv$display_df <- cleaned_data ##don't revert to orig data for cleaning
    } else if(input$dupes_group == "c") {
      rv$display_df <- rv$original_df ##revert to orig data for cleaning
    } else {
      # do nothing
    }
    output$data_table <- renderDataTable({
      df <- rv$display_df ##don't revert to orig data for cleaning
      if (is.null(df)) {
        datatable(data.frame(Error = "No data"), rownames = FALSE) ##don't revert to orig data for cleaning
      } else {
        col_rename <- c(
          "registrant.name" = "registrant",
          "client.name" = "client",
          "filing_type_display" = "filing type",
          "dt_posted" = "date posted",
          "filing_document_url" = "filing url",
          "registrant.description" = "registrant description",
          "client.general_description" = "client description",
          "filing_type" = "filing type",
          "filing_period" = "filing period"
        )
        df <- as.data.frame(df) ##don't revert to orig data for cleaning
        old_names <- names(df) ##don't revert to orig data for cleaning
        names(df) <- ifelse(old_names %in% names(col_rename), col_rename[old_names], old_names) ##don't revert to orig data for cleaning
        if ("filing url" %in% names(df)) {
          df[["filing url"]] <- ifelse(
            is.na(df[["filing url"]]) | df[["filing url"]] == "",
            "",
            sprintf('<a href="%s" target="_blank">%s</a>', df[["filing url"]], df[["filing url"]])
          )
        }
        if ("income" %in% names(df)) df$income <- as.numeric(gsub("[^0-9.-]", "", df$income))
        if ("expenses" %in% names(df)) df$expenses <- as.numeric(gsub("[^0-9.-]", "", df$expenses))
        dt <- datatable(
          df,
          options = list(
            pageLength = -1,
            scrollX = TRUE,
            scrollY = "600px",
            dom = 'Bfrtip'
          ),
          filter = 'top',
          rownames = FALSE,
          escape = FALSE
        )
        if ("income" %in% names(df)) {
          dt <- DT::formatCurrency(dt, "income", currency = "$", digits = 2)
        }
        if ("expenses" %in% names(df)) {
          dt <- DT::formatCurrency(dt, "expenses", currency = "$", digits = 2)
        }
        dt
      }
    })
  }) #end flag dupes functionality

  #### flag conflict radio button-related functionality
  observeEvent(input$conflict_group, {
    req(rv$display_df) ##don't revert to orig data for cleaning
    if(input$conflict_group == "a") {
      cleaned_data <- flag_client_registrant_conflict(rv$display_df, flag_conflict = TRUE, clean_doublecounts = FALSE) ##don't revert to orig data for cleaning
      rv$display_df <- cleaned_data ##don't revert to orig data for cleaning
    } else if(input$conflict_group == "b") {
      cleaned_data <- flag_client_registrant_conflict(rv$display_df, flag_conflict = TRUE, clean_doublecounts = TRUE) ##don't revert to orig data for cleaning
      rv$display_df <- cleaned_data ##don't revert to orig data for cleaning
    } else if(input$conflict_group == "c") {
      rv$display_df <- rv$original_df ##don't revert to orig data for cleaning
    } else {
      # do nothing
    }
    output$data_table <- renderDataTable({
      df <- rv$display_df ##don't revert to orig data for cleaning
      if (is.null(df)) {
        datatable(data.frame(Error = "No data"), rownames = FALSE) ##don't revert to orig data for cleaning
      } else {
        col_rename <- c(
          "registrant.name" = "registrant",
          "client.name" = "client",
          "filing_type_display" = "filing type",
          "dt_posted" = "date posted",
          "filing_document_url" = "filing url",
          "registrant.description" = "registrant description",
          "client.general_description" = "client description",
          "filing_type" = "filing type",
          "filing_period" = "filing period"
        )
        df <- as.data.frame(df) ##don't revert to orig data for cleaning
        old_names <- names(df) ##don't revert to orig data for cleaning
        names(df) <- ifelse(old_names %in% names(col_rename), col_rename[old_names], old_names) ##don't revert to orig data for cleaning
        if ("filing url" %in% names(df)) {
          df[["filing url"]] <- ifelse(
            is.na(df[["filing url"]]) | df[["filing url"]] == "",
            "",
            sprintf('<a href="%s" target="_blank">%s</a>', df[["filing url"]], df[["filing url"]])
          )
        }
        if ("income" %in% names(df)) df$income <- as.numeric(gsub("[^0-9.-]", "", df$income))
        if ("expenses" %in% names(df)) df$expenses <- as.numeric(gsub("[^0-9.-]", "", df$expenses))
        dt <- datatable(
          df,
          options = list(
            pageLength = -1,
            scrollX = TRUE,
            scrollY = "600px",
            dom = 'Bfrtip'
          ),
          filter = 'top',
          rownames = FALSE,
          escape = FALSE
        )
        if ("income" %in% names(df)) {
          dt <- DT::formatCurrency(dt, "income", currency = "$", digits = 2)
        }
        if ("expenses" %in% names(df)) {
          dt <- DT::formatCurrency(dt, "expenses", currency = "$", digits = 2)
        }
        dt
      }
    })
  }) # end flag conflict functionality

  ####### Reset all form and helper inputs when the Reset button is clicked
  observeEvent(input$reset, {
    shinyjs::reset("myapp")           # Clears main inputs
    shinyjs::reset("cleaning_helpers") # Clears conflict/duplicate helpers
  })

  ####### Toggle advanced search fields visibility
  observeEvent(input$toggleAdvanced, {
    shinyjs::toggle(id = "advanced", anim = TRUE)
  })

  ####### Toggle conflict checker visibility
  observeEvent(input$toggleconflict, {
    shinyjs::toggle(id = "conflict", anim = TRUE)
  })

  ####### Toggle duplicate checker visibility
  observeEvent(input$toggledupes, {
    shinyjs::toggle(id = "dupes", anim = TRUE)
  })

  ####### Toggle API key management section visibility
  observeEvent(input$toggleAPI, {
    shinyjs::toggle(id = "api_key_section", anim = TRUE)
  })

  ####### Toggle math helper visibility
  observeEvent(input$toggleMath, {
    shinyjs::toggle(id = "math_dropdown", anim = TRUE)
  })

  ####### Math functionality: sum totals and show in a modal dialog
  observeEvent(input$sum_totals, {
    # Use cleaned data if available, else use result_df
    df <- if (!is.null(rv$display_df)) rv$display_df else rv$result_df ##don't revert to orig data for cleaning
    if (is.null(df)) {
      showModal(modalDialog(
        title = "No Data",
        "No data available to sum. Please run a query first.",
        easyClose = TRUE
      ))
      return()
    }
    # Ensure columns are numeric
    if ("income" %in% names(df)) df$income <- as.numeric(gsub("[^0-9.-]", "", df$income))
    if ("expenses" %in% names(df)) df$expenses <- as.numeric(gsub("[^0-9.-]", "", df$expenses))
    # Calculate sums, handling missing columns
    income_sum <- if ("income" %in% names(df)) sum(df$income, na.rm = TRUE) else NA
    expenses_sum <- if ("expenses" %in% names(df)) sum(df$expenses, na.rm = TRUE) else NA
    # Format with commas
    income_sum_fmt <- if (!is.na(income_sum)) format(income_sum, big.mark = ",", scientific = FALSE, digits = 2, nsmall = 2) else NA
    expenses_sum_fmt <- if (!is.na(expenses_sum)) format(expenses_sum, big.mark = ",", scientific = FALSE, digits = 2, nsmall = 2) else NA
    # Create message
    msg <- paste0(
      if (!is.na(income_sum_fmt)) sprintf("Total Income: $%s\n", income_sum_fmt) else "",
      if (!is.na(expenses_sum_fmt)) sprintf("Total Expenses: $%s", expenses_sum_fmt) else ""
    )
    showModal(modalDialog(
      title = "Sum Totals",
      pre(msg),
      easyClose = TRUE
    ))
  })

  ## end math section

  ####### API key management: trigger for checking key existence
  key_trigger <- reactiveVal(0)

  ####### Helper to check if a Senate API key exists in the keyring
  key_exists <- reactive({
    key_trigger()
    tryCatch({
      keyring::key_get("senate_api_key")
      TRUE
    }, error = function(e) FALSE)
  })

  ####### Handle saving the API key to the keyring
  observeEvent(input$save_key, {
    req(input$api_key_input)
    if (nchar(input$api_key_input) < 20) {
      output$status <- renderText("Error: Key appears too short (min 20 chars)")
      status_message("Error: Key appears too short (min 20 chars)") ##status_message_fix
      return()
    }
    tryCatch({
      keyring::key_set_with_value(
        service = "senate_api_key",
        username = NULL,
        password = input$api_key_input
      )
      output$status <- renderText("Success: API key saved!")
      status_message("Success: API key saved!") ##status_message_fix
      key_trigger(key_trigger() + 1)
    }, error = function(e) {
      output$status <- renderText(paste("Error:", e$message))
      status_message(paste("Error:", e$message)) ##status_message_fix
    })
  })
  ####### Handle verifying the API key
  observeEvent(input$verify_key, {
    if (key_exists()) {
      output$status <- renderText("Key exists and is valid")
      status_message("Key exists and is valid") ##status_message_fix
    } else {
      output$status <- renderText("No key found. Please set one first.")
      status_message("No key found. Please set one first.") ##status_message_fix
    }
  })
  ####### Handle deleting the API key, with confirmation modal
  observeEvent(input$delete_key, {
    if (key_exists()) {
      showModal(modalDialog(
        title = "Are you sure?",
        "This will delete your API key from the system keychain. Are you sure you want to continue?",
        footer = tagList(
          actionButton("confirm_delete", "Delete", class = "btn-danger"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    } else {
      output$status <- renderText("No key exists to delete")
      status_message("No key exists to delete") ##status_message_fix
    }
  })

  ####### Actually delete the API key if user confirms
  observeEvent(input$confirm_delete, {
    tryCatch({
      keyring::key_delete("senate_api_key")
      output$status <- renderText("Key successfully deleted")
      status_message("Key successfully deleted") ##status_message_fix
      key_trigger(key_trigger() + 1)
      removeModal()
    }, error = function(e) {
      output$status <- renderText(paste("Error deleting key:", e$message))
      status_message(paste("Error deleting key:", e$message)) ##status_message_fix
      removeModal()
    })
  })

  ####### Download handler for exporting all results as CSV
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("lobbyr_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(rv$display_df)) {
        write.csv(rv$display_df, file, row.names = FALSE)
      } else {
        # Optionally handle the case where no data is available
        write.csv(data.frame(), file)
      }
    }
  )                                 ##add download all button

} #end server handling

####### Launch the Shiny app with the defined UI and server
shinyApp(ui = ui, server = server)
