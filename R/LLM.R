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

## LLM helper function

LLM <- function(prompt = prompt,
                LLM_model = "gpt-4o",
                max_tokens = 2000,
                temperature = 0,
                # suffix = NULL,
                top_p = 1,
                logprobs = TRUE,
                top_logprobs = 5,
                # stop = NULL,
                # presence_penalty = 0,
                # frequency_penalty = 0,
                timeout_sec = 60,
                system_prompt = NULL,
                raw_output = TRUE,
                update_key = update_key){


  if (LLM_model == "llama-3") {
    api_key <- get_api_key("togetherai",
                           update_key = update_key)

    # API endpoint
    endpoint <- "https://api.together.xyz/v1/chat/completions"
    model <- "meta-llama/Llama-3-70b-chat-hf"

    # Request body
    request_body <- list(model = model,
                         max_tokens = max_tokens,
                         messages = list(
                           list(role = "system", content = system_prompt),
                           list(role = "user", content = prompt)
                         ),
                         temperature = temperature,
                         logprobs = logprobs)



    # Make the API request
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           httr::add_headers(Authorization = paste("Bearer", api_key),
                                             `Content-Type` = "application/json"),
                           encode = "json",
                           times = 5,
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)
    output <- content$choices[[1]]$message$content


  } else if (LLM_model == "mixtral") {
    api_key <- get_api_key("huggingface",
                           update_key = update_key)


    # API endpoint
    endpoint <- "https://api-inference.huggingface.co/models/mistralai/Mixtral-8x7B-Instruct-v0.1/v1/chat/completions"
    model <- "mistralai/Mixtral-8x7B-Instruct-v0.1"

    if (logprobs == TRUE) {
      # Request body
      request_body <- list(model = model,
                           max_tokens = max_tokens,
                           messages = list(
                             list("role" = "system", "content" = system_prompt),
                             list("role" = "user", "content" = prompt)
                           ),
                           temperature = temperature,
                           logprobs = logprobs,
                           top_logprobs = top_logprobs)


    } else if (logprobs == FALSE) {
      # Request body
      request_body <- list(model = model,
                           max_tokens = max_tokens,
                           messages = list(
                             list("role" = "system", "content" = system_prompt),
                             list("role" = "user", "content" = prompt)
                           ),
                           temperature = temperature,
                           logprobs = logprobs)

    }

    # Make the API request
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           httr::add_headers(Authorization = paste("Bearer", api_key),
                                             `Content-Type` = "application/json"),
                           encode = "json",
                           times = 5,
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)
    output <- content$choices[[1]]$message$content

  } else if (LLM_model == "gpt-4o" | LLM_model == "gpt-4" | LLM_model == "gpt-4-turbo" | LLM_model == "gpt-3.5-turbo") {

    api_key <- get_api_key("openai",
                           update_key = update_key)

    # API endpoint
    endpoint <- "https://api.openai.com/v1/chat/completions"

    if (logprobs == TRUE) {
      # Request body
      request_body <- list(
        model = LLM_model,
        max_tokens = max_tokens,
        temperature = temperature,
        # n = 1,
        # suffix = suffix,
        # top_p = top_p,
        top_logprobs = top_logprobs,
        logprobs = logprobs,
        # stop = stop,
        # presence_penalty = presence_penalty,
        # frequency_penalty = frequency_penalty,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = prompt)
        )
      )

    } else if (logprobs == FALSE) {
      # Request body
      request_body <- list(
        model = LLM_model,
        max_tokens = max_tokens,
        temperature = temperature,
        # n = 1,
        # suffix = suffix,
        # top_p = top_p,
        logprobs = logprobs,
        # stop = stop,
        # presence_penalty = presence_penalty,
        # frequency_penalty = frequency_penalty,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = prompt)
        )
      )

    }

    # Make the API request
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           httr::add_headers(Authorization = paste("Bearer",
                                                                   api_key)),
                           encode = "json",
                           times = 5,
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)
    output <- content$choices[[1]]$message$content

  } else if (LLM_model == "gpt-4.1") {
    logprob = FALSE

    api_key <- get_api_key("openai",
                           update_key = update_key)

    # API endpoint
    endpoint <- "https://api.openai.com/v1/responses"

    # Request body
    request_body <- list(
      model = LLM_model,
      tools = list(list(type = "web_search_preview")),
      max_output_tokens = max_tokens,
      temperature = temperature,
      # n = 1,
      # suffix = suffix,
      # top_p = top_p,
      # stop = stop,
      # presence_penalty = presence_penalty,
      # frequency_penalty = frequency_penalty,
      input = prompt
    )


    # Make the API request
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           httr::add_headers(Authorization = paste("Bearer",
                                                                   api_key)),
                           encode = "json",
                           times = 5,
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)
    output <- content$output[[2]]$content[[1]]$text

    n_sources <- length(raw_content$output[[2]]$content[[1]]$annotations)

    sources <- NULL
    for (i in 1:n_sources) {
      sources[i] <- content$output[[2]]$content[[1]]$annotations[[i]]$url

    }

  }

  if (LLM_model == "gpt-4.1") {
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$output[[2]]$content[[1]]$text,
                                      finish_reason = raw_content$output[[2]]$status,
                                      prompt_tokens = raw_content$usage$input_tokens,
                                      answer_tokens = raw_content$usage$output_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   sources = sources,
                   output = output)

  } else if (raw_output == TRUE && logprobs == TRUE) {
    top5_logprobs <- LLM_logprobs(raw_content = raw_content,
                                  LLM_model = LLM_model)
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$choices[[1]]$message$content,
                                      finish_reason = raw_content$choices[[1]]$finish_reason,
                                      prompt_tokens = raw_content$usage$prompt_tokens,
                                      answer_tokens = raw_content$usage$completion_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   top5_tokens = top5_logprobs,
                   output = output)


  } else if (raw_output == TRUE && logprobs == FALSE) {
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$choices[[1]]$message$content,
                                      finish_reason = raw_content$choices[[1]]$finish_reason,
                                      prompt_tokens = raw_content$usage$prompt_tokens,
                                      answer_tokens = raw_content$usage$completion_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   output = output)

  } else if (raw_output == FALSE && logprobs == TRUE) {
    top5_logprobs <- LLM_logprobs(raw_content = raw_content,
                                  LLM_model = LLM_model)
    output <- list(top5_tokens = top5_logprobs,
                   output = output)


  }

  return(output)
}
