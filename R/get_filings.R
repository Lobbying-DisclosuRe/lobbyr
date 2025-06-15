#' #' Search lobbying disclosure data
#'
#' This function allows users to search the Senate federal lobbying disclosures database (https://lda.senate.gov/api/redoc/v1/)
#' to ascertain much companies are spending to lobby the federal government or how much lobbyists are being paid on behalf of clients
#' to lobby on various issues. The query searches based on various criteria including issues, filing periods, client names, registrant
#'  names, and dates.
#'
#'
#' @param issues A character vector of specific issues you want to search for.
#' @param issue_joiner A character string specifying how to combine the `issue` terms -- Only ("and" or "or") can be used.
#' @param year A character string specifying the year to search within for results. -- Only one year can be used.
#' @param filing_period  A character string specifying the filing period. -- Only the following can be used "first_quarter"
#'  "second_quarter" "third_quarter" "fourth_quarter". Very old filings sometimes can only be separated by filing period
#'  "mid_year" "year_end", but using those isn't recommended.
#' @param client_name A character string specifying a client that a lobbyist was lobbying on behalf of.
#' @param registrant_name A character string specifying the name of the entity that was registered to lobby.
#' @param starting_date A character string specifying the start date for the search (Format is - YYYY-MM-DD).
#' @param ending_date A character string specifying the end date for the search (Format is - YYYY-MM-DD).
#' @param tidy_result A logical value indicating whether to return a simplified version of the returned data.
#' @param ignore_disclaimer A logical value indicating whether to ignore the disclaimer explaining the limitations of the data.
#'
#' @return  A data frame containing the search results.
#' @export
#' @examples
#'
#' get raw third quarter filings with ANY of the terms filed in the second half of 2024 (note this may require you to set your end date in 2025)
#'
#' df <- get_filings(
#'  issues = c("tax", "trade", "health"),
#'  issue_joiner =  "or",
#   year = 2024, # can be used for only one year at a time
#'  filing_period = "third_quarter",
#'  ending_date = "2024-12-31", # format yyyy-mm-dd #filings sometimes are amended long after a quarter is over, so make sure you
#'                                                  are setting your date to allow for that. Otherwise, you can just not use a
#'                                                  start/ending date at all.
#'  starting_date = "2024-06-01", # format yyyy-mm-dd
#'  tidy_result = FALSE,
#'  ignore_disclaimer = FALSE
#'  )
#'
#'get a cleaner version of filings from the Chamber of Commerce that include ALL of the terms specified
#'
#' df <- get_filings(
#'  issues = c("tax", "trade", "health"),
#'  issue_joiner =  "and",
#'  client_name = "Chamber of Commerce of the U.S.A.",
#'  registrant_name = "Chamber of Commerce of the U.S.A.",
#'  tidy_result = TRUE,
#'  ignore_disclaimer = FALSE
#'  )
#'
#'
#'
get_filings <- function(issues = c(""),
                        issue_joiner = "",
                        year = "",
                        filing_period = "",
                        client_name = "",
                        registrant_name = "",
                        starting_date = "",
                        ending_date = "",
                        tidy_result = TRUE,
                        ignore_disclaimer = FALSE) {
  ######### take specific issues and combine it in a proper format to ping the API
  # specific_issues <- function(lobbying_issues = "", join_word = "") { ### corrected
  #   # Wrap each term in double quotes
  #   terms <- paste0('"', lobbying_issues, '"')
  #   # Define the join string
  #   joiner <- switch(tolower(join_word),
  #     "and" = "+AND+", # if the user supplies and which returns disclosures with all terms
  #     "or"  = "OR", # if the user supplies or which returns disclosures with any terms (Note no + sign in this function)
  #     join_word # fallback
  #   )
  #   # Combine terms and handle empty case
  #   query_string <- if (length(terms) > 0 && nchar(terms[1]) > 0) {
  #     paste0(terms, collapse = joiner)
  #   } else {
  #     ""
  #   }
  #
  #   # Add single quotes and check for empty quoted string
  #   query_string <- paste0("'", query_string, "'")
  #   if (query_string == "'\"\"'") query_string <- ""
  #
  #   return(query_string)
  # }
  # # generate the issues we'll use in the next step
  #
  # several_issues <- specific_issues(what_issue[[1]], what_issue[[2]])

  ####### new version

  specific_issues <- function(lobbying_issues = "", join_word = "") {
    # Handle comma-separated strings and convert to vector
    if (length(lobbying_issues) == 1 && grepl(",", lobbying_issues)) {  ### corrected
      lobbying_issues <- unlist(strsplit(lobbying_issues, ",\\s*"))
    }
    # Wrap each term in double quotes
    terms <- paste0('"', lobbying_issues, '"')
    # Define the join string
    joiner <- switch(tolower(join_word),
                     "and" = "+AND+",
                     "or"  = "OR",
                     join_word
    )
    # Combine terms and handle empty case
    query_string <- if (length(terms) > 0 && nchar(terms[1]) > 0) {
      paste0(terms, collapse = joiner)
    } else {
      ""
    }
    # Add single quotes and check for empty quoted string
    query_string <- paste0("'", query_string, "'")
    if (query_string == "'\"\"'") query_string <- ""
    return(query_string)
  }

  several_issues <- specific_issues(issues, issue_joiner)


  ####### end new version

  # provide the base and filings urls
  base_url <- "https://lda.senate.gov"
  filings <- "/api/v1/filings/"

  # build the request based on what is provided
  req <- request(base_url) |>
    req_url_path(filings) |>
    req_headers(Authorization = paste0("Token ", keyring::key_get("senate_api_key"))) |>
    req_url_query(
      filing_specific_lobbying_issues = several_issues,
      filing_year = year,
      registrant_name = registrant_name,
      filing_period = filing_period, # what filing period are we searching for?
      client_name = client_name, # which client lobbyists are lobbying on behalf of are we looking for
      filing_dt_posted_before = ending_date, # the end date we're searching for
      filing_dt_posted_after = starting_date # the start date we're searching for
    ) |>
    req_throttle(rate = 120 / 60)


  resps <- req_perform_iterative(
    req |> req_url_query(page_size = 25),
    next_req = iterate_with_offset(
      "page",
      start = 1, # start at the first page
      offset = 1, # go up by one page each time
      resp_pages = function(resp) ceiling(resp_body_json(resp)$count / 25) # takes resp and returns the total number of pages, or NULL if unknown. It will only be called once.
    ),
    max_reqs = Inf
  )
  # save the result in case we want to use it again later
  write_rds(resps, "resps_request_result.rds")

  # simple function to unpack each api result and bring to level of the responses

  unpackedhopeitworks <- resps |>
    resps_successes() |>
    resps_data(function(resp) resp_body_json(resp, check_type = TRUE, simplifyVector = TRUE, flatten = TRUE)$results) # flatten and simplify vector doing big lifts here to turn this into a df

  ### added later to expand the data to bring up each lobbying area and what the filer said about the issues they lobbied on in a given lobbying area to make it easier to potentially seek like terms and maybe fuzzy match at some point areas of interest ###

  data_to_work_with <- unpackedhopeitworks |>
    hoist(
      lobbying_activities,
      general_issue_code_display = "general_issue_code_display",
      description = "description"
    ) %>%
    unnest(c(general_issue_code_display, description), keep_empty = TRUE) |>
    group_by(registrant.name, client.name) |>
    pivot_wider(
      names_from = general_issue_code_display,
      values_from = description,
      values_fn = list
    )

  # added as a cleaning function see wrangle data ctd section - basically just reducing the large multi-column response with the raw filing data to a very minimal format

  cleaner_view <- function(dataframe_i_want_to_use, tidy_up_response = TRUE) {
    if (tidy_up_response == TRUE) {
      dataframe_i_want_to_use |>
        select(any_of(c("registrant.name", "client.name", "filing_type_display", "income", "expenses", "filing_year", "dt_posted", "filing_document_url", "registrant.description", "client.general_description", "filing_type", "filing_period")))
    } else {
      return(dataframe_i_want_to_use)
    }
  }

  # return the tidied dataframe
  cleaned_data_to_work_with <- cleaner_view(data_to_work_with, tidy_up_response = tidy_result)


  # Print disclaimer at the end - this can be muted with mute messages -
  ignore_message <- function(mute_message = FALSE) {
    if (mute_message == FALSE) {
      message("DISCLAIMER: This data is known to contain errors and requires additional filtering and cleaning to ensure correct results.")
      message("See documentation for more guidance and filtering examples.")
      message()
      message("FACT CHECKING:")
      message("+If you're looking to fact-check, use the filing document url to look at the source of the information as it was filed.")
      message("+Ensure that there is only one filing for a given registrant in each filing_period for each year to avoid double counting the amount spent or earned on lobbying.")
      message()
      message("DOUBLE COUNTING:")
      message("If, for example, in the same quarter of a year an entity has a filing called '1st Quarter - Report', '1st Quarter - Termination' and '1st Quarter - Amendment', you must make sure to only count one of those (the latest is usually the most accurate) otherwise you risk double counting.")
      message("+The helper column called, 'double count risk' should have insights into some of these instances, but it's not perfect. So, double check.")
      message("+Registrations and terminations are separate from quarterly lobby spending and must be filtered out to determine an entity's yearly spending on lobbying.")
      message()
      message("MORE HELPFUL HINTS:")
      message("+If an entity name appears as a registrant, but also appears as a client. Do not sum the values. Instead, use the value in the registrant's expenses field to gauge the amount spent on lobbying by the registrant.")
      message()
      message("SOURCE: Federal lobbying disclosures maintained in the U.S. Senate Lobbying Disclosure Act Database and queried through the official Lobbying Disclosure REST API v1 - Read more here - https://lda.senate.gov/api/redoc/v1/")
    } else {
      message("Disclaimer is muted. But you should read it, and can do that by removing ignore_disclaimer = TRUE from DisclosuR call")
    }
  }

  # ignore the message if the disclaimer ignore option is set to true
  ignore_message(mute_message = ignore_disclaimer)

  # return the finished data from API
  return(cleaned_data_to_work_with)
}  ## end function
