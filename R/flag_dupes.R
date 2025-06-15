#' Flag and Optionally Clean Duplicate or Dubious Lobbying Filings
#'
#'Identifies and flags potentially problematic or duplicate lobbying filings in a data frame
#'(typically one returned by \code{get_filings()}). The function adds several diagnostic columns to help users spot
#'filings that may require closer inspection, and can optionally remove all but the latest filing for each
#'egistrant-client-quarter group.
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
#' @return A data frame with additional columns indicating potential issues, and (optionally) with duplicate filings removed.
#' @export
#' @examples
#'
#' # Flag and clean duplicate filings in a lobbying data frame
#'
#' dupes_flag_test_clean <- flag_dupes(df, find_duplicates = TRUE, attempt_cleaning = TRUE)
#'
#' # Only flag, do not remove duplicates
#'
#' flagged_only <- flag_dupes(df, find_duplicates = TRUE, attempt_cleaning = FALSE)
#'
#' @seealso \code{\link{get_filings}}
flag_dupes <- function(cleaned_dataframe_from_previous_function, find_duplicates = TRUE, attempt_cleaning = TRUE) {
  if (find_duplicates) {
    dupes_flagged <- cleaned_dataframe_from_previous_function |>
      mutate(
        registration_or_termination = str_detect(filing_type, regex("RR$|T$", ignore_case = TRUE)),
        quarter_number = str_extract(filing_type, "\\d+") |> as.integer(),
        is_amendment = str_detect(filing_type, "\\d+A$")
      ) |>
      group_by(registrant.name, client.name, filing_year, quarter_number) |>
      mutate(
        has_quarter = any(!is_amendment & !is.na(quarter_number)), # make sure we're flagging registrations by eliminating na and those that are not amendments
        has_amendment = any(is_amendment),
        registration_termination = any(registration_or_termination), # if the filling is a termination or a registration it would be easier to exclude them or carefully check them for accuracy
        is_duplicate = (duplicated(income) | duplicated(income, fromLast = TRUE) | duplicated(expenses) | duplicated(expenses, fromLast = TRUE)), # if the income or expenses column is duplicated, this could suggest there are two identical filings in a given quarter
        checkme = if_else(has_quarter & has_amendment, "CHECK", if_else(!has_quarter & !has_amendment & !is_amendment, "CHECK", if_else(registration_termination | is_amendment | is_duplicate, "CHECK", "PASS CHECK")))
      ) |>
      ungroup()
  } else {
    dupes_flagged <- cleaned_dataframe_from_previous_function
  }

  # Clean attempt function moved outside nested logic
  clean_attempt <- function(dataframe_with_flagged_dupes) {
    dataframe_with_flagged_dupes |>
      filter(!registration_or_termination) |>
      mutate(dt_posted = as.POSIXct(dt_posted)) |>
      group_by(registrant.name, client.name, filing_year, filing_period) |>
      arrange(desc(dt_posted)) |>
      slice_tail(n = 1) |> #select the last result (in this case, the latest filing which is typically the amended version)
      ungroup()
  }

  if (attempt_cleaning) {
    cleaned_and_flagged_dataframe <- clean_attempt(dupes_flagged)
  } else {
    cleaned_and_flagged_dataframe <- dupes_flagged
  }
  message("This function either removed or identified lobbying filings that, if left in, could lead to doublecounting of spending on lobbying. It is not perfect. Please see documentation on tips for fact-checking these by hand.")
  return(cleaned_and_flagged_dataframe)
}
