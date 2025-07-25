% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_senate_api_key.R
\name{set_senate_api_key}
\alias{set_senate_api_key}
\title{Set and Store Disclosure API Key}
\usage{
set_senate_api_key(api_key)
}
\arguments{
\item{api_key}{Character. Optional. If provided, sets the API key directly. If omitted, the function prompts the user to enter the key.}
}
\value{
Invisibly returns \code{NULL}. The API key is stored securely in the system keyring.
}
\description{
Prompts the user to enter their U.S. Senate Lobbying Disclosure API key and securely stores it using the \code{keyring} package. The stored key is used by [get_filings()] for authentication.
}
\details{
This function uses \code{keyring::key_set()} to securely store the API key under the name "senate_api_key". The function pauses briefly to allow the user to read instructions before prompting for input.

#' API keys can be requested at: \url{https://lda.senate.gov/api/register/}
}
\examples{
# Set or update the API key (prompts user for input)
set_senate_api_key()

# Set the API key directly (not recommended for interactive use)
# set_senate_api_key("your_api_key_here")

}
\seealso{
[keyring::key_set()] for secure key storage, and [get_filings()] for using the stored API key.
}
