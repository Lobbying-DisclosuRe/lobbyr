pkgname <- "lobbyR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('lobbyR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("flag_client_registrant_conflict")
### * flag_client_registrant_conflict

flush(stderr()); flush(stdout())

### Name: flag_client_registrant_conflict
### Title: Identify and Resolve Potential Double-Counting of
###   Client/Registrant
### Aliases: flag_client_registrant_conflict

### ** Examples

## Not run: 
##D # Flag and clean potential double-counting cases
##D conflict_removed_and_flagged <- flag_client_registrant_conflict(
##D   cleaner_df, 
##D   flag_conflict = TRUE, 
##D   clean_doublecounts = TRUE
##D )
##D 
##D # Only flag, do not remove
##D conflict_flagged_only <- flag_client_registrant_conflict(
##D   cleaner_df, 
##D   flag_conflict = TRUE, 
##D   clean_doublecounts = FALSE
##D )
## End(Not run)




cleanEx()
nameEx("flag_dupes")
### * flag_dupes

flush(stderr()); flush(stdout())

### Name: flag_dupes
### Title: Flag and Clean Duplicate or Dubious Lobbying Filings
### Aliases: flag_dupes

### ** Examples

## Not run: 
##D # Flag and clean duplicate filings in a lobbying data frame
##D dupes_flag_test <- flag_dupes(
##D   df, 
##D   find_duplicates = TRUE, 
##D   attempt_cleaning = TRUE
##D )
##D 
##D # Only flag, do not remove duplicates
##D flagged_only <- flag_dupes(
##D   df, 
##D   find_duplicates = TRUE, 
##D   attempt_cleaning = FALSE
##D )
## End(Not run)




cleanEx()
nameEx("get_filings")
### * get_filings

flush(stderr()); flush(stdout())

### Name: get_filings
### Title: Query Federal Lobbying Disclosures
### Aliases: get_filings

### ** Examples

## Not run: 
##D # Get raw third quarter filings that include any of the terms "tax", "trade", "health"
##D df <- get_filings(
##D   issues = c("tax", "trade", "health"),
##D   issue_joiner = "or",
##D   year = 2024, # can be used for only one year at a time
##D   filing_period = "third_quarter",
##D   tidy_result = FALSE,
##D   ignore_disclaimer = FALSE
##D )
##D 
##D # Get a cleaner version of filings produced by the 7 Eleven and the lobbyists who 7 Eleven hired
##D # that include ALL of the terms specified
##D cleaner_df <- get_filings(
##D   issues = c("fees", "foods", "immigration"),
##D   issue_joiner = "and",
##D   client_name = "7 Eleven, Inc.",
##D   tidy_result = TRUE,
##D   ignore_disclaimer = FALSE
##D )
##D 
##D 
##D # Query filings for tax, company, or bill issues in the first quarter
##D # for a specific client/registrant - NOTE: this probably won't yield much
##D # in the way of results, but it uses every available parameter so you can
##D # see how they're used.
##D df <- get_filings(
##D   issues = c("tax", "company", "bill"),
##D   issue_joiner = "or",
##D   filing_period = "first_quarter",
##D   client_name = "Chamber of Commerce of the U.S.A.",
##D   registrant_name = "Chamber of Commerce of the U.S.A.",
##D   ending_date = "2025-01-25",
##D   starting_date = "2021-04-01",
##D    min_amount = "", ## min/max,
##D     max_amount = ""  ## min/max,
##D   tidy_result = TRUE,
##D   ignore_disclaimer = TRUE
##D )
## End(Not run)



cleanEx()
nameEx("set_senate_api_key")
### * set_senate_api_key

flush(stderr()); flush(stdout())

### Name: set_senate_api_key
### Title: Set and Store Disclosure API Key
### Aliases: set_senate_api_key

### ** Examples

## Not run: 
##D # Set or update the API key (prompts user for input)
##D set_senate_api_key()
##D 
##D # Set the API key directly (not recommended for interactive use)
##D # set_senate_api_key("your_api_key_here")
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
