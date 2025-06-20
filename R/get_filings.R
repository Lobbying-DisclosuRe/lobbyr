#'  Query Federal Lobbying Disclosures
#'
#' @description
#' 'get_filings()' allows users to search the Senate federal lobbying disclosures database (https://lda.senate.gov/api/redoc/v1/)
#' to ascertain much companies are spending to lobby the federal government or how much lobbyists are being paid on behalf of clients
#' to lobby on various issues. The query searches based on various criteria including issues, filing periods, client names, registrant
#'  names, and dates.
#'
#' @param issues  Optional, but when using multiple search terms, must be paired with iissue_joiner. Character vector of issues to search for (e.g., \code{c("tax", "energy", "OECD")}). Each term will be wrapped in double quotes and joined using \code{issue_joiner}.
#' @param issue_joiner  Optional, but must be used for multiple issues. Character string, either \code{"and"} or \code{"or"}, specifying whether to require all terms or any term in the search.
#' @param year  Optional. A character string specifying the year to search for results. -- Only one year can be used.
#' @param filing_period   Optional. A character string limiting results to a specific filing period. -- Only the following can be used "first_quarter" "second_quarter" "third_quarter" "fourth_quarter". Very old filings sometimes can only be separated by filing period "mid_year" "year_end", but using those isn't recommended.
#' @param client_name  Optional. A character string specifying the client an entity that filed its own disclosure, or the client a lobbyist discloses that it lobbied on behalf of.
#' @param registrant_name Optional. Filter results to filings for a specific registrant (lobbying firm or self-filer).
#' @param starting_date Optional. A character string specifying the start date for the search (Format is - YYYY-MM-DD).
#' @param ending_date Optional. A character string specifying the end date for the search (Format is - YYYY-MM-DD).
#' @param tidy_result Optional. A logical value that if \code{TRUE} (default), returns a reduced, tidy data frame with key columns. If \code{FALSE}, returns all available columns from api query.
#' @param ignore_disclaimer Optional. If \code{TRUE}, suppresses the printed disclaimer and guidance messages explaining the limitations of the data.
#'
#' @details
#' This function queries the U.S. Senate Lobbying Disclosure API for filings that match the user-supplied criteria. Users can search by issue, client, registrant, filing period, and date range. The function automatically formats issue terms for the API and provides options to return a tidy or full data frame.
#'
#' The returned data frame includes columns for registrant, client, filing type, income, expenses, year, date posted, document URL, and descriptions of lobbying activities by issue area. If \code{tidy_result = TRUE}, only a subset of columns is returned for easier analysis.
#'
#' A disclaimer message is printed by default, reminding users to fact-check results and avoid double-counting. Set \code{ignore_disclaimer = TRUE} to suppress this message.
#'
#' @returns A data frame of lobbying filings matching the query, with lobbying activities unnested and issues as columns. If \code{tidy_result = TRUE}, the data frame is reduced to key columns.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get raw third quarter filings that include any of the terms "tax", "trade", "health"
#' df <- get_filings(
#'   issues = c("tax", "trade", "health"),
#'   issue_joiner = "or",
#'   year = 2024, # can be used for only one year at a time
#'   filing_period = "third_quarter",
#'   tidy_result = FALSE,
#'   ignore_disclaimer = FALSE
#' )
#'
#' # Get a cleaner version of filings produced by the US Chamber of Commerce
#' # that include ALL of the terms specified
#' df <- get_filings(
#'   issues = c("tax", "trade", "health"),
#'   issue_joiner = "and",
#'   client_name = "Chamber of Commerce of the U.S.A.",
#'   registrant_name = "Chamber of Commerce of the U.S.A.",
#'   tidy_result = TRUE,
#'   ignore_disclaimer = FALSE
#' )
#'
#' # Query filings for tax, company, or bill issues in the first quarter
#' # for a specific client/registrant - NOTE: this probably won't yield much
#' # in the way of results, but it uses every available parameter so you can
#' # see how they're used.
#' bigger_temp <- get_filings(
#'   issues = c("tax", "company", "bill"),
#'   issue_joiner = "or",
#'   filing_period = "first_quarter",
#'   client_name = "Chamber of Commerce of the U.S.A.",
#'   registrant_name = "Chamber of Commerce of the U.S.A.",
#'   ending_date = "2025-01-25",
#'   starting_date = "2021-04-01",
#'   tidy_result = TRUE,
#'   ignore_disclaimer = TRUE
#' )
#' }
#'@seealso [set_senate_api_key()] for entering and storing the disclosures api key, [flag_dupes()] for handling duplicate filings or amendments filed in the same quarter that can cause doublecounting, and [flag_client_registrant_conflict()] for methods to prevent doublecounting when entities that file lobbying disclosures as registrants, but pay outside lobbying firms too, also show up as clients.
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
  specific_issues <- function(lobbying_issues = "", join_word = "") {
    if (length(lobbying_issues) == 1 && grepl(",", lobbying_issues)) {
      lobbying_issues <- unlist(strsplit(lobbying_issues, ",\\s*"))
    }
    terms <- paste0('"', lobbying_issues, '"')
    joiner <- switch(tolower(join_word),
                     "and" = "+AND+",
                     "or"  = "OR",
                     join_word
    )
    query_string <- if (length(terms) > 0 && nchar(terms[1]) > 0) {
      paste0(terms, collapse = joiner)
    } else {
      ""
    }
    query_string <- paste0("'", query_string, "'")
    if (query_string == "'\"\"'") query_string <- ""
    return(query_string)
  }

  several_issues <- specific_issues(issues, issue_joiner)

  #function that checks for an api key
  get_api_key <- function() {
    tryCatch(
      {
        keyring::key_get("senate_api_key")
      },
      error = function(e) {
        stop("No API key found in keyring. Please run set_senate_api_key() first.")
      }
    )
  }
  api_key <- get_api_key()

  base_url <- "https://lda.senate.gov"
  filings <- "api/v1/filings/"

  req <- httr2::request(base_url) |>
    httr2::req_url_path(filings) |>
    httr2::req_headers(Authorization = paste0("Token ", api_key)) |>
    httr2::req_url_query(
      filing_specific_lobbying_issues = several_issues,
      filing_year = year,
      registrant_name = registrant_name,
      filing_period = filing_period,
      client_name = client_name,
      filing_dt_posted_before = ending_date,
      filing_dt_posted_after = starting_date
    ) |>
    httr2::req_throttle(rate = 120 / 60)


  resps <- withCallingHandlers(
    #with calling handlers used to throw a more verbose error on 404
    httr2::req_perform_iterative(
      req |> httr2::req_url_query(page_size = 25),
      next_req = httr2::iterate_with_offset(
        "page",
        start = 1,
        offset = 1,
        resp_pages = function(resp) {
          count <- httr2::resp_body_json(resp)$count
          if (is.null(count) || count < 1) {
            stop("The API response counts 0 records returned, or is missing for another reason. Please make sure you have entered at least one query parameter, and they're entered in the correct format and don't conflict. If you've done that and are still having issues, data for this query may be unavailable. Further troubleshooting can be done through the GUI https://lda.senate.gov/filings/public/filing/search/")
          }
          ceiling(count / 25)
        }
      ),
      max_reqs = Inf
    ),
    httr2_http_404 = function(cnd) {
      rlang::abort("Connection with API unsuccessful. A more detailed error is possible by wrapping your get_filings() query thustly: 'httr2::with_verbosity(get_filings(add your arguments), verbosity = 2)' For more common issues visit https://lda.senate.gov/api/redoc/v1/#section/Common-Errors", parent = cnd)
    },
    httr2_http_400 = function(cnd){
      rlang::abort("400 Bad Request typically due to invalid query string parameter values. A more detailed error is possible by wrapping your get_filings() query thustly: 'httr2::with_verbosity(get_filings(add your arguments), verbosity = 2)' For more common issues visit https://lda.senate.gov/api/redoc/v1/#section/Common-Errors", parent = cnd)
    },
    httr2_http_429 = function(cnd){
      rlang::abort("Rate limit exceeded. Function throttling set for users with API key. A more detailed error is possible by wrapping your get_filings() query thustly: 'httr2::with_verbosity(get_filings(add your arguments), verbosity = 2)' For more common issues visit https://lda.senate.gov/api/redoc/v1/#section/Common-Errors", parent = cnd)
    },
    httr2_http_405 = function(cnd){
      rlang::abort("unsupported HTTP method. A more detailed error is possible by wrapping your get_filings() query thustly: 'httr2::with_verbosity(get_filings(add your arguments), verbosity = 2)' For more common issues visit https://lda.senate.gov/api/redoc/v1/#section/Common-Errors", parent = cnd)
    }
  )
  # readr::write_rds(resps, "resps_request_result.rds") # this can be utilized to save the results locally. Turned off, unless needed

  #take response and unpack, clean it up

  unpackedhopeitworks <- resps |>
    httr2::resps_successes() |>
    httr2::resps_data(function(resp) httr2::resp_body_json(resp, check_type = TRUE, simplifyVector = TRUE, flatten = TRUE)$results)
  data_to_work_with <- unpackedhopeitworks |>
    tidyr::hoist(
      lobbying_activities,
      general_issue_code_display = "general_issue_code_display",
      description = "description"
    ) |>
    tidyr::unnest(c(general_issue_code_display, description), keep_empty = TRUE) |>
    dplyr::group_by(registrant.name, client.name) |>
    tidyr::pivot_wider(
      names_from = general_issue_code_display,
      values_from = description,
      values_fn = list
    )
  cleaner_view <- function(dataframe_i_want_to_use, tidy_up_response = TRUE) {
    if (tidy_up_response == TRUE) {
      dataframe_i_want_to_use |>
        dplyr::select(dplyr::any_of(c("registrant.name", "client.name", "filing_type_display", "income", "expenses", "filing_year", "dt_posted", "filing_document_url", "registrant.description", "client.general_description", "filing_type", "filing_period")))
    } else {
      return(dataframe_i_want_to_use)
    }
  }
  cleaned_data_to_work_with <- cleaner_view(data_to_work_with, tidy_up_response = tidy_result)
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
  ignore_message(mute_message = ignore_disclaimer)
  return(cleaned_data_to_work_with)
} ## end function
