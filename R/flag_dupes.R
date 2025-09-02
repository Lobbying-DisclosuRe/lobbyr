#' Flag and Clean Duplicate or Dubious Lobbying Filings
#'
#' @description
#' 'flag_dupes()'Identifies and flags potentially problematic or duplicate lobbying filings in a data frame
#' (typically one returned by \code{get_filings()}). The function adds several diagnostic columns to help users spot
#' filings that may require closer inspection, and can optionally remove all but the latest filing for each
#' egistrant-client-quarter group.
#'
#' @param cleaned_dataframe_from_previous_function - A data frame, typically the output of \code{get_filings()}, containing lobbying filings to be checked for duplicates or other issues.
#' @param find_duplicates Logical. If \code{TRUE} (default), the function flags dubious filings using several heuristics and regex patterns.
#' @param attempt_cleaning Logical. If \code{TRUE} (default), the function removes all but the latest filing for each registrant-client-quarter group, assuming the most recent filing is the most accurate.
#'
#' @details
#' The function creates several columns to help identify filings that may be of concern:
#' \itemize{
#'   \item \code{registration_or_termination}: TRUE if the filing is a registration or termination filing (detected via regex).
#'   \item \code{quarter_number}: Extracted quarter number from \code{filing_type}.
#'   \item \code{is_amendment}: TRUE if the filing is an amendment.
#'   \item \code{has_quarter}, \code{has_amendment}, \code{registration_termination}, \code{is_duplicate}: Various flags for duplicate or suspicious filings.
#'   \item \code{checkme}: "CHECK" if the row is flagged as potentially problematic, otherwise "PASS CHECK".
#' }
#' If \code{attempt_cleaning = TRUE}, the function keeps only the latest filing (by \code{dt_posted}) for each registrant, client, year, and filing period, after removing registration and termination filings.
#'
#' @returns A data frame with additional columns indicating potential issues, and (optionally) with duplicate filings removed.
#' @export
#' @examples
#' \dontrun{
#' # Flag and clean duplicate filings in a lobbying data frame
#' dupes_flag_test <- flag_dupes(
#'   df, 
#'   find_duplicates = TRUE, 
#'   attempt_cleaning = TRUE
#' )
#'
#' # Only flag, do not remove duplicates
#' flagged_only <- flag_dupes(
#'   df, 
#'   find_duplicates = TRUE, 
#'   attempt_cleaning = FALSE
#' )
#' }
#'
#' @seealso [get_filings()] for retrieving data from the API, and [flag_client_registrant_conflict] for methods to prevent doublecounting when entities that file lobbying disclosures as registrants, but pay outside lobbying firms too, also show up as clients.
flag_dupes <- function(cleaned_dataframe_from_previous_function, find_duplicates = TRUE, attempt_cleaning = TRUE) {
  if (find_duplicates) {
    dupes_flagged <- cleaned_dataframe_from_previous_function |>
      dplyr::mutate(
        registration_or_termination = stringr::str_detect(filing_type, stringr::regex("RR$|T$", ignore_case = TRUE)),
        quarter_number = stringr::str_extract(filing_type, "\\d+") |> as.integer(),
        is_amendment = stringr::str_detect(filing_type, "\\d+A$")
      ) |>
      dplyr::group_by(registrant.name, client.name, filing_year, quarter_number) |>
      dplyr::mutate(
        has_quarter = any(!is_amendment & !is.na(quarter_number)),
        has_amendment = any(is_amendment),
        registration_termination = any(registration_or_termination),
        is_duplicate = (duplicated(income) | duplicated(income, fromLast = TRUE) | duplicated(expenses) | duplicated(expenses, fromLast = TRUE)),
        checkme = dplyr::if_else(
          has_quarter & has_amendment, "CHECK",
          dplyr::if_else(!has_quarter & !has_amendment & !is_amendment, "CHECK",
                         dplyr::if_else(registration_termination | is_amendment | is_duplicate, "CHECK", "PASS CHECK"))
        )
      ) |>
      dplyr::ungroup()
  } else {
    dupes_flagged <- cleaned_dataframe_from_previous_function
  }
  clean_attempt <- function(dataframe_with_flagged_dupes) {
    dataframe_with_flagged_dupes |>
      dplyr::filter(!registration_or_termination) |>
      dplyr::mutate(dt_posted = as.POSIXct(dt_posted)) |>
      dplyr::group_by(registrant.name, client.name, filing_year, filing_period) |>
      dplyr::arrange(dplyr::desc(dt_posted)) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  }
  if (attempt_cleaning) {
    cleaned_and_flagged_dataframe <- clean_attempt(dupes_flagged)
  } else {
    cleaned_and_flagged_dataframe <- dupes_flagged
  }
  message("This function either removed or identified lobbying filings that, if left in, could lead to doublecounting of spending on lobbying. It is not perfect. Please see documentation on tips for fact-checking these by hand.")
  return(cleaned_and_flagged_dataframe)
} # end of function
