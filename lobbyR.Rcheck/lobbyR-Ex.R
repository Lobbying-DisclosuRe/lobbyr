pkgname <- "lobbyR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "lobbyR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('lobbyR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("flag_client_registrant_conflict")
### * flag_client_registrant_conflict

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flag_client_registrant_conflict
### Title: Identify and Resolve Potential Double-Counting of
###   Client/Registrant
### Aliases: flag_client_registrant_conflict

### ** Examples

# Flag and clean potential double-counting cases
flagged_conflict <- flag_client_registrant_conflict(bigger_temp, flag_conflict = TRUE, clean_doublecounts = TRUE)

# Only flag, do not remove
flagged_only <- flag_client_registrant_conflict(bigger_temp, flag_conflict = TRUE, clean_doublecounts = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flag_client_registrant_conflict", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flag_dupes")
### * flag_dupes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flag_dupes
### Title: Flag and Clean Duplicate or Dubious Lobbying Filings
### Aliases: flag_dupes

### ** Examples


# Flag and clean duplicate filings in a lobbying data frame

dupes_flag_test <- flag_dupes(df, find_duplicates = TRUE, attempt_cleaning = TRUE)

# Only flag, do not remove duplicates

flagged_only <- flag_dupes(df, find_duplicates = TRUE, attempt_cleaning = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flag_dupes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_filings")
### * get_filings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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
##D # Get a cleaner version of filings produced by the US Chamber of Commerce
##D # that include ALL of the terms specified
##D df <- get_filings(
##D   issues = c("tax", "trade", "health"),
##D   issue_joiner = "and",
##D   client_name = "Chamber of Commerce of the U.S.A.",
##D   registrant_name = "Chamber of Commerce of the U.S.A.",
##D   tidy_result = TRUE,
##D   ignore_disclaimer = FALSE
##D )
##D 
##D # Query filings for tax, company, or bill issues in the first quarter
##D # for a specific client/registrant - NOTE: this probably won't yield much
##D # in the way of results, but it uses every available parameter so you can
##D # see how they're used.
##D bigger_temp <- get_filings(
##D   issues = c("tax", "company", "bill"),
##D   issue_joiner = "or",
##D   filing_period = "first_quarter",
##D   client_name = "Chamber of Commerce of the U.S.A.",
##D   registrant_name = "Chamber of Commerce of the U.S.A.",
##D   ending_date = "2025-01-25",
##D   starting_date = "2021-04-01",
##D   tidy_result = TRUE,
##D   ignore_disclaimer = TRUE
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_filings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("set_senate_api_key")
### * set_senate_api_key

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: set_senate_api_key
### Title: Set and Store Disclosure API Key
### Aliases: set_senate_api_key

### ** Examples

# Set or update the API key (prompts user for input)
set_senate_api_key()

# Set the API key directly (not recommended for interactive use)
# set_senate_api_key("your_api_key_here")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("set_senate_api_key", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
