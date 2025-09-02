#' Set and Store Disclosure API Key
#'
#' @description
#' Prompts the user to enter their U.S. Senate Lobbying Disclosure API key and securely stores it using the \code{keyring} package. The stored key is used by [get_filings()] for authentication.
#'
#' @param api_key Character. Optional. If provided, sets the API key directly. If omitted, the function prompts the user to enter the key.
#'
#' @details
#' This function uses \code{keyring::key_set()} to securely store the API key under the name "senate_api_key". The function pauses briefly to allow the user to read instructions before prompting for input.
#'
#' #' API keys can be requested at: \url{https://lda.senate.gov/api/register/}
#'
#' @returns Invisibly returns \code{NULL}. The API key is stored securely in the system keyring.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set or update the API key (prompts user for input)
#' set_senate_api_key()
#'
#' # Set the API key directly (not recommended for interactive use)
#' # set_senate_api_key("your_api_key_here")
#' }
#'
#' @seealso [keyring::key_set()] for secure key storage, and [get_filings()] for using the stored API key.
set_senate_api_key <- function(api_key) {
  message("enter api key when password request appears")
  message("API keys can be requested here https://lda.senate.gov/api/register/")
  message("Pausing for 5 seconds so you can read...")
  Sys.sleep(5)
  message("Ok, now you can enter your api key")
  keyring::key_set("senate_api_key")
} # end of function
