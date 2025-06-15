
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lobbyR

<!-- badges: start -->

<!-- badges: end -->

The goal of lobbyR is to provide a suite of tools for querying,
cleaning, and analyzing U.S. Senate Lobbying Disclosure Act (LDA) data
via the official REST API. It is designed for journalists, researchers,
and transparency advocates who want to explore federal lobbying
disclosures in a reproducible and programmatic way. The package includes
helpers for searching by issue, client, registrant, and date, as well as
for flagging duplicates, identifying client-registrant conflicts, and
securely storing your API key.

## Installation

You can install the development version of lobbyR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Lobbying-DisclosuRe/lobbyr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lobbyR)

# Set your API key (you'll be prompted to enter it securely)
if (FALSE) { # \dontrun{
  # just doing this so it doesn't run
  set_senate_api_key()
} # }

# Query filings for tax, company, or bill issues in the first quarter for a specific client/registrant
seven_eleven_filings <- get_filings(
issues = c("fees", "foods", "immigration"),
issue_joiner =  "or",
 client_name = "7 Eleven, Inc.",
  ending_date = "2025-1-25", # format yyyy-mm-dd
  starting_date = "2020-04-01", # format yyyy-mm-dd
  tidy_result = T,
  ignore_disclaimer = F
)
#> Iterating ■■■■■■■■■                         25% | ETA:  3sIterating ■■■■■■■■■■■■■■■■                  50% | ETA:  2sIterating ■■■■■■■■■■■■■■■■■■■■■■■           75% | ETA:  1s                                                           DISCLAIMER: This data is known to contain errors and requires additional filtering and cleaning to ensure correct results.
#> See documentation for more guidance and filtering examples.
#> 
#> FACT CHECKING:
#> +If you're looking to fact-check, use the filing document url to look at the source of the information as it was filed.
#> +Ensure that there is only one filing for a given registrant in each filing_period for each year to avoid double counting the amount spent or earned on lobbying.
#> 
#> DOUBLE COUNTING:
#> If, for example, in the same quarter of a year an entity has a filing called '1st Quarter - Report', '1st Quarter - Termination' and '1st Quarter - Amendment', you must make sure to only count one of those (the latest is usually the most accurate) otherwise you risk double counting.
#> +The helper column called, 'double count risk' should have insights into some of these instances, but it's not perfect. So, double check.
#> +Registrations and terminations are separate from quarterly lobby spending and must be filtered out to determine an entity's yearly spending on lobbying.
#> 
#> MORE HELPFUL HINTS:
#> +If an entity name appears as a registrant, but also appears as a client. Do not sum the values. Instead, use the value in the registrant's expenses field to gauge the amount spent on lobbying by the registrant.
#> 
#> SOURCE: Federal lobbying disclosures maintained in the U.S. Senate Lobbying Disclosure Act Database and queried through the official Lobbying Disclosure REST API v1 - Read more here - https://lda.senate.gov/api/redoc/v1/

# Flag and clean duplicate filings
dupes_flag_test <- flag_dupes(seven_eleven_filings, find_duplicates = TRUE, attempt_cleaning = TRUE)
#> This function either removed or identified lobbying filings that, if left in, could lead to doublecounting of spending on lobbying. It is not perfect. Please see documentation on tips for fact-checking these by hand.

# Flag and remove potential double-counting between registrant and client
flagged_conflict <- flag_client_registrant_conflict(seven_eleven_filings, flag_conflict = TRUE, clean_doublecounts = TRUE)
```

## Main Functions

### `get_filings()`

- Query the U.S. Senate LDA API for lobbying filings.
- Search by issue(s), client, registrant, filing period, and date range.
- Returns a cleaned and optionally tidied data frame.
- Includes a detailed disclaimer and fact-checking guidance.

#### Example

``` r

chamber_df <- get_filings(
  issues = c("tax", "trade", "bill"),
  issue_joiner = "or",
  filing_period = "first_quarter",
  client_name = "Chamber of Commerce of the U.S.A.",
  registrant_name = "Chamber of Commerce of the U.S.A.",
  ending_date = "2025-01-25",
  starting_date = "2015-04-01",
  tidy_result = TRUE,
  ignore_disclaimer = TRUE
)
#> ⠙ Iterating 1 done (0.43/s) | 2.3sIterating ■■■■■                             14% | ETA: 15sIterating ■■■■■■■■■■                        29% | ETA: 15sIterating ■■■■■■■■■■■■■■                    43% | ETA: 12sIterating ■■■■■■■■■■■■■■■■■■                57% | ETA:  8sIterating ■■■■■■■■■■■■■■■■■■■■■■            71% | ETA:  6sIterating ■■■■■■■■■■■■■■■■■■■■■■■■■■■       86% | ETA:  3s                                                           Disclaimer is muted. But you should read it, and can do that by removing ignore_disclaimer = TRUE from DisclosuR call
```

### `flag_dupes()`

- Flags filings that may be duplicates, amendments, or otherwise
  dubious.
- Adds diagnostic columns to help identify problematic filings.
- Optionally removes all but the latest filing for each
  registrant-client-quarter group.

#### Example

``` r
dupes_flag_test <- flag_dupes(chamber_df, find_duplicates = TRUE, attempt_cleaning = TRUE)
#> This function either removed or identified lobbying filings that, if left in, could lead to doublecounting of spending on lobbying. It is not perfect. Please see documentation on tips for fact-checking these by hand.
```

### `flag_client_registrant_conflict()`

- Flags and (optionally) removes filings that could result in
  double-counting, such as when an entity files as both registrant and
  client.
- Adds a flag column indicating which filings are likely to be
  duplicates.

#### Example

``` r
flagged_conflict <- flag_client_registrant_conflict(chamber_df, flag_conflict = TRUE, clean_doublecounts = TRUE)
```

### `set_senate_api_key()`

- Securely store your U.S. Senate LDA API key using the `keyring`
  package.
- Prompts you to enter your key, which is then used by `get_filings()`.

#### Example

``` r

  set_senate_api_key()
```

------------------------------------------------------------------------

## Data Cleaning and Best Practices

- Always review flagged filings and potential double-counts by hand for
  critical analyses. There’s lots of cases that don’t get caught,
  unfortunately. For instance the 7 Eleven filing was spelled three
  different ways - “7-ELEVEN, INC”, “7 ELEVEN, INC.” and “7-ELEVEN,
  INC.”
- Use the `checkme` and `flag` columns to guide manual review. But
  there’s always other cases. Feel free to flag and report them to me.
- Only sum lobbying expenses for a registrant once per quarter/year to
  avoid double-counting.
- Consult the filing document URL for source verification.

------------------------------------------------------------------------

## Disclaimer

> This data is known to contain errors and requires additional filtering
> and cleaning to ensure correct results. See documentation for more
> guidance and filtering examples. If you’re looking to fact-check, use
> the filing document URL to review the source. Registrations and
> terminations are separate from quarterly lobby spending and must be
> filtered out to determine an entity’s yearly spending on lobbying. If
> an entity appears as both registrant and client, do not sum the
> values; instead, use the registrant’s expenses field to gauge total
> lobbying spend.

------------------------------------------------------------------------

## API Key

- API keys can be requested at: <https://lda.senate.gov/api/register/>
- Store your key securely with `set_senate_api_key()`.

------------------------------------------------------------------------

## Dependencies

- `httr2`
- `dplyr`
- `tidyr`
- `stringr`
- `keyring`
- `readr`
- `purrr`

------------------------------------------------------------------------

## License

GNU LGPLv3

------------------------------------------------------------------------

## Contributing

Pull requests and issues are welcome. Please see the repository for
guidelines.

------------------------------------------------------------------------

## Citation

Working on this still If you use this package in your research or
reporting, please cite the data from the U.S. Senate Lobbying Disclosure
Act Database and this package.

------------------------------------------------------------------------

## Contact

For questions, issues, or suggestions, please visit:
<https://github.com/yourusername/lobbyR>

------------------------------------------------------------------------

## Acknowledgments

Thanks to the U.S. Congress for providing open data and the guidance of
American University’s Aarushi Sahejpal. Thanks to AI tools and the R
community for help and how-tos in creating some regexes and code
patterns.

<div style="text-align: center">

⁂

</div>
