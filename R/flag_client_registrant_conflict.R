#' Identify and Resolve Potential Double-Counting of Client/Registrant
#'
#' @description
#' Flags instances where an entity filed as a registrant (which, if following the rules, should include all lobbying spending by that entity) but also appeared as a client in filings by outside lobbyists. This helps avoid double-counting lobbying expenses.
#'
#' @param dataframe_that_i_determine A data frame, typically the name of the output from [get_filings()], containing lobbying filings for a given query.
#' @param flag_conflict Logical. If \code{TRUE} (default), the function flags filings that are likely duplicates as described above.
#' @param clean_doublecounts Logical. If \code{TRUE} (default), the function automatically filters out cases that could lead to double-counting.
#'
#' @details
#' This function identifies:
#' \itemize{
#'   \item Self-lobbying entities (where registrant and client names match), which should include all lobbying spending.
#'   \item Cases where the same entity appears as a client in filings by other lobbyists, which should be excluded from aggregate totals to avoid double-counting.
#' }
#' The function adds a flag column to the data frame, indicating whether each filing is a "Report of all entity's spending", "Likely part of separate entity's report", or "No entity's report detected". If \code{clean_doublecounts = TRUE}, filings that are likely to cause double-counting are removed.
#'
#' Note: This function is not perfect and may miss some edge cases. Manual review is recommended for critical analyses.
#'
#' @returns A data frame with a new flag column and, optionally, with potential double-counting cases filtered out.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Flag and clean potential double-counting cases
#' conflict_removed_and_flagged <- flag_client_registrant_conflict(
#'   cleaner_df, 
#'   flag_conflict = TRUE, 
#'   clean_doublecounts = TRUE
#' )
#'
#' # Only flag, do not remove
#' conflict_flagged_only <- flag_client_registrant_conflict(
#'   cleaner_df, 
#'   flag_conflict = TRUE, 
#'   clean_doublecounts = FALSE
#' )
#' }
#'
#' @seealso [get_filings()] for retrieving lobbying data, and [flag_dupes] for identifying duplicate or amended filings.
flag_client_registrant_conflict <- function(dataframe_that_i_determine, flag_conflict = TRUE, clean_doublecounts = TRUE) {
  if (flag_conflict) {
    #add cleaned rows I need later
    dataframe_that_i_determine <- dataframe_that_i_determine |>
      ##added: Create cleaned comparison columns with whitespace trimming and all spaces removed to try and compensate for lobbyists using different punctuation, or spaces with the names of their clients...
      dplyr::mutate(
        registrant_clean = stringr::str_remove_all(
          stringr::str_squish(stringr::str_to_lower(stringr::str_remove_all(registrant.name, "[[:punct:]]"))),
          "\\s"
        ),
        client_clean = stringr::str_remove_all(
          stringr::str_squish(stringr::str_to_lower(stringr::str_remove_all(client.name, "[[:punct:]]"))),
          "\\s"
        )
      )
    self_lobbying_entities <- dataframe_that_i_determine |>
      dplyr::filter(registrant_clean == client_clean) |>
      dplyr::distinct(entity = registrant_clean)

    cross_lobbying_cases <- dataframe_that_i_determine |>
      dplyr::filter(client_clean %in% self_lobbying_entities$entity) |>
      dplyr::anti_join(self_lobbying_entities, by = c("registrant_clean" = "entity"))

    flagged_cases <- dataframe_that_i_determine |>
      dplyr::mutate(
        flag = dplyr::case_when(
          registrant_clean == client_clean ~ "Report of all entity's spending",
          client_clean %in% self_lobbying_entities$entity ~ "Likely part of separate entity's report",
          TRUE ~ "No entity's report detected"
        )
      )
  }else {
    flagged_cases <- dataframe_that_i_determine
  }
  remove_client_registrant_conflict <- function(dataframe_i_cleaned_above) {
    no_client_registrant_conflict <- dataframe_i_cleaned_above |>
      dplyr::filter(flag != "Likely part of separate entity's report")
  }
  if (clean_doublecounts) {
    filtered_dataframe_with_flagged_conflict_col <- remove_client_registrant_conflict(flagged_cases)
  } else {
    filtered_dataframe_with_flagged_conflict_col <- flagged_cases
  }
  message("This function either removed or identified lobbying filings that, if left in, could lead to doublecounting of spending on lobbying. It is not perfect. Please see documentation on tips for fact-checking these by hand.")
  return(filtered_dataframe_with_flagged_conflict_col)
}# end of function
