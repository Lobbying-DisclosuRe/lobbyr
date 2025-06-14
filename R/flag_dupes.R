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
