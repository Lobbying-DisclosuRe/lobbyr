flag_client_registrant_conflict <- function(dataframe_that_i_determine, flag_conflict = TRUE, clean_doublecounts = TRUE) {
  if (flag_conflict) {
    # 1. Identify self-lobbying entities (registrant = client)
    self_lobbying_entities <- dataframe_that_i_determine |>
      filter(registrant.name == client.name) |>
      distinct(entity = registrant.name) # find distinct entities that report all lobbying in one report

    # 2. Find cross-lobbying cases (same entity as client with different registrant)
    cross_lobbying_cases <- dataframe_that_i_determine |>
      filter(client.name %in% self_lobbying_entities$entity) |>
      anti_join(self_lobbying_entities, by = c("registrant.name" = "entity"))

    # 3. Flag both cases in original data
    flagged_cases <- dataframe_that_i_determine |>
      mutate(
        flag = case_when(
          registrant.name == client.name ~ "Report of all entity's spending",
          client.name %in% self_lobbying_entities$entity ~ "Likely part of separate entity's report",
          TRUE ~ "No entity's report detected"
        )
      )
  } else {
    flagged_cases <- return(dataframe_that_i_determine)
  }
  # second function that filters out the rows part of the entity's report
  remove_client_registrant_conflict <- function(dataframe_i_cleaned_above) {
    no_client_registrant_conflict <- dataframe_i_cleaned_above |>
      filter(flag != "Likely part of separate entity's report")
  }

  if (clean_doublecounts) {
    filtered_dataframe_with_flagged_conflict_col <- remove_client_registrant_conflict(flagged_cases)
  } else {
    filtered_dataframe_with_flagged_conflict_col <- return(flagged_cases)
  }
  return(filtered_dataframe_with_flagged_conflict_col)
  message("This function either removed or identified lobbying filings that, if left in, could lead to doublecounting of spending on lobbying. It is not perfect. Please see documentation on tips for fact-checking these by hand.")
}
