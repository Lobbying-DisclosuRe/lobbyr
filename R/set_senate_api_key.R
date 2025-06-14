# enter api key when password request appears
set_senate_api_key <- function(api_key) {
  message("enter api key when password request appears")
  message("API keys can be requested here https://lda.senate.gov/api/register/")
  message("Pausing for 5 seconds so you can read...")
  Sys.sleep(5) # Pause for 3 seconds
  message("Ok, now you can enter your api key")
  key_set("senate_api_key")
}
