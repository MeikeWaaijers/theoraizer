# Program Name: theoraizer
# Description: This function interacts with Large Language Model (LLM) APIs and extracts their output.
# Copyright (C) <2024> <Meike Waaijers>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

### theoraizer

## get_api_key helper function

get_api_key <- function(service_name, update_key = FALSE) {
  # Check if running on shinyapps.io
  shinyapps <- Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps"
  # Check if running shiny app
  if (shinyapps) {
    # Attempt to retrieve the API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")

    if (nzchar(api_key)) {
      return(api_key)
    }
  }

  # Check if running in a CI environment
  ci <- nzchar(Sys.getenv("CI"))

  if (ci) {
    # Attempt to retrieve the API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")

    if (nzchar(api_key)) {
      return(api_key)
    }
  }

  # If not found in environment variable and not in CI, attempt to retrieve from keyring
  if (!ci && !shinyapps && (update_key || nrow(keyring::key_list(service = service_name)) == 0)) {
    cat("To use this functionality, an API key needs to be set.\n")
    cat("Please follow these steps to resolve the issue:\n")
    if (service_name == "anyscale") {
      cat("1. Create an API key on https://app.endpoints.anyscale.com/credentials \n")
    } else if (service_name == "openai") {
      cat("1. Create an API key on https://platform.openai.com/account/api-keys \n")
    }
    cat("2. Please enter your API key below to add/update it.")
    answer <- readline("API key = ")
    keyring::key_set_with_value(service = service_name, username = "user", password = answer)
  }

  return(keyring::key_get(service = service_name, username = "user"))
}
