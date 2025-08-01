% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_filings.R
\name{get_filings}
\alias{get_filings}
\title{Query Federal Lobbying Disclosures}
\usage{
get_filings(
  issues = c(""),
  issue_joiner = "",
  year = "",
  filing_period = "",
  client_name = "",
  registrant_name = "",
  starting_date = "",
  ending_date = "",
  tidy_result = TRUE,
  ignore_disclaimer = FALSE
)
}
\arguments{
\item{issues}{Optional, but when using multiple search terms, must be paired with iissue_joiner. Character vector of issues to search for (e.g., \code{c("tax", "energy", "OECD")}). Each term will be wrapped in double quotes and joined using \code{issue_joiner}.}

\item{issue_joiner}{Optional, but must be used for multiple issues. Character string, either \code{"and"} or \code{"or"}, specifying whether to require all terms or any term in the search.}

\item{year}{Optional. A character string specifying the year to search for results. -- Only one year can be used.}

\item{filing_period}{Optional. A character string limiting results to a specific filing period. -- Only the following can be used "first_quarter" "second_quarter" "third_quarter" "fourth_quarter". Very old filings sometimes can only be separated by filing period "mid_year" "year_end", but using those isn't recommended.}

\item{client_name}{Optional. A character string specifying the client an entity that filed its own disclosure, or the client a lobbyist discloses that it lobbied on behalf of.}

\item{registrant_name}{Optional. Filter results to filings for a specific registrant (lobbying firm or self-filer).}

\item{starting_date}{Optional. A character string specifying the start date for the search (Format is - YYYY-MM-DD).}

\item{ending_date}{Optional. A character string specifying the end date for the search (Format is - YYYY-MM-DD).}

\item{tidy_result}{Optional. A logical value that if \code{TRUE} (default), returns a reduced, tidy data frame with key columns. If \code{FALSE}, returns all available columns from api query.}

\item{ignore_disclaimer}{Optional. If \code{TRUE}, suppresses the printed disclaimer and guidance messages explaining the limitations of the data.}
}
\value{
A data frame of lobbying filings matching the query, with lobbying activities unnested and issues as columns. If \code{tidy_result = TRUE}, the data frame is reduced to key columns.
}
\description{
'get_filings()' allows users to search the Senate federal lobbying disclosures database (https://lda.senate.gov/api/redoc/v1/)
to ascertain much companies are spending to lobby the federal government or how much lobbyists are being paid on behalf of clients
to lobby on various issues. The query searches based on various criteria including issues, filing periods, client names, registrant
 names, and dates.
}
\details{
This function queries the U.S. Senate Lobbying Disclosure API for filings that match the user-supplied criteria. Users can search by issue, client, registrant, filing period, and date range. The function automatically formats issue terms for the API and provides options to return a tidy or full data frame.

The returned data frame includes columns for registrant, client, filing type, income, expenses, year, date posted, document URL, and descriptions of lobbying activities by issue area. If \code{tidy_result = TRUE}, only a subset of columns is returned for easier analysis.

A disclaimer message is printed by default, reminding users to fact-check results and avoid double-counting. Set \code{ignore_disclaimer = TRUE} to suppress this message.
}
\examples{
\dontrun{
# Get raw third quarter filings that include any of the terms "tax", "trade", "health"
df <- get_filings(
  issues = c("tax", "trade", "health"),
  issue_joiner = "or",
  year = 2024, # can be used for only one year at a time
  filing_period = "third_quarter",
  tidy_result = FALSE,
  ignore_disclaimer = FALSE
)

# Get a cleaner version of filings produced by the 7 Eleven and the lobbyists who 7 Eleven hired
# that include ALL of the terms specified
cleaner_df <- get_filings(
  issues = c("fees", "foods", "immigration"),
  issue_joiner = "and",
  client_name = "7 Eleven, Inc.",
  tidy_result = TRUE,
  ignore_disclaimer = FALSE
)


# Query filings for tax, company, or bill issues in the first quarter
# for a specific client/registrant - NOTE: this probably won't yield much
# in the way of results, but it uses every available parameter so you can
# see how they're used.
df <- get_filings(
  issues = c("tax", "company", "bill"),
  issue_joiner = "or",
  filing_period = "first_quarter",
  client_name = "Chamber of Commerce of the U.S.A.",
  registrant_name = "Chamber of Commerce of the U.S.A.",
  ending_date = "2025-01-25",
  starting_date = "2021-04-01",
  tidy_result = TRUE,
  ignore_disclaimer = TRUE
)
}
}
\seealso{
[set_senate_api_key()] for entering and storing the disclosures api key, [flag_dupes()] for handling duplicate filings or amendments filed in the same quarter that can cause doublecounting, and [flag_client_registrant_conflict()] for methods to prevent doublecounting when entities that file lobbying disclosures as registrants, but pay outside lobbying firms too, also show up as clients.
}
