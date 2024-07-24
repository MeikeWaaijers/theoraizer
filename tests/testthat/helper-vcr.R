library(vcr)
library(keyring)

vcr_dir <- vcr::vcr_test_path("fixtures")

# Use the updated get_api_key function
api_key <- get_api_key("openai")

if (!nzchar(api_key)) {
  if (dir.exists(vcr_dir)) {
    # Fake API token to fool our package
    api_key <- "foobar"
  } else {
    # If there's no mock files nor API token, impossible to run tests
    stop("No API key nor cassettes, tests cannot be run.",
         call. = FALSE)
  }
}

# Configure vcr
invisible(vcr::vcr_configure(
  dir = vcr_dir,
  record = "once",
  filter_request_headers = list(Authorization = "My bearer token is safe", "User-Agent"),
  filter_response_headers = list("Set-Cookie", "Date"),
  serialize_with = "json",  # Use JSON instead of YAML
  preserve_exact_body_bytes = FALSE,
  match_requests_on = c("method", "uri")
))
