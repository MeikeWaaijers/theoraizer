# Program Name: theoraizer
# Description: In the var_list function a Large Language Model (LLM) is instructed to generate a list of important variables for a particular context.
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


#### theoraizer
### Variable listing function

## Function manual

#' Create a Variable List
#'
#' @description
#' In \code{var_list()} a Large Language Model (LLM) is instructed to generate a list of important variables for a particular context.
#'
#' The function prompts a LLM to compile two lists of variables with 2 different prompts.
#' The LLM is instructed to include only variables that are precisely defined and measured on a binary or continuous scale.
#' The LLM then merges the two lists, removing duplicates and synonyms, and reviews the combined list to identify any missing crucial variables.
#' Finally, it refines the list by separating combined factors into distinct items.
#'
#' In addition, users can limit the final number of variables, prompting the LLM to select the N most important variables.
#'
#' @usage
#' var_list(context,
#'          include_context = FALSE,
#'          n_final = Inf,
#'          n_variables = "all",
#'          LLM_model = "gpt-4o",
#'          max_tokens = 2000,
#'          update_key = FALSE)
#' @details
#' To create a fully fledged theory from scratch, the functions in this R-packaged should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}}
#'
#' @param context A character vector specifying the context for which a theory should be developed. If it is not feasible to identify a particular context, the argument can be set to NULL.
#' @param include_context If \code{include_context = FALSE} (default), the context specified in the \code{"context"} argument will not be included as a seperate variable in the variable list.
#' @param n_variables Number of variables the LLM should generate in the first 2 variable lists. If \code{"all"} (default), the LLM is not limited to generate a specific number of variables, but is asked to create a list of "all" important variables.
#' @param n_final Number of variables to be included in the final variable list. If \code{inf} (default), the final integrated variable list will not be limited to a certain number of variables.
#' @param LLM_model The LLM model that should be used to generate output: \code{"gpt-4o"} (default), \code{"gpt-4"}, \code{"gpt-4-turbo"} \code{"gpt-3.5-turbo"}, \code{"llama-3"} (specifically points to Llama-3-70B-Chat-hf, accessed via Hugging Face), or \code{"mixtral"} (specifically points to Mixtral-8x7B-Instruct-v0.1, accessed via Together.ai).
#' @param max_tokens The maximum number of tokens the LLM should generate. Be careful when adjusting this argument. Reducing the maximum token limit will reduce the cost but may result in incomplete answers. Conversely, increasing the token limit can be advantageous for obtaining more detailed responses. The maximum number of tokens depends on the model (\code{6000} for \code{"gpt-4o"}, \code{"gpt-4"}, and \code{"gpt-4-turbo"}, \code{3000} for \code{"gpt-3.5-turbo"}, and \code{2000} for \code{"mixtral"}).
#' @param update_key If \code{update_key = TRUE}, the function will prompt the user for a new API key and update the saved key. If \code{update_key = FALSE} (default), the function will use the existing API key if available.
#'
#' @returns
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'     \itemize{
#'       \item \code{function_part}: Part of function from which LLM output originates.
#'       \item \code{iteration}: Iteration number.
#'       \item \code{LLM_model}: LLM model used.
#'       \item \code{prompt}: Prompt used.
#'       \item \code{system_prompt}: System prompt used.
#'       \item \code{content}: Unprocessed LLM output.
#'       \item \code{finish_reason}: Reason the LLM stopped generating output.
#'       \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
#'       \item \code{answer_tokens}: Number of tokens used for the LLM answer.
#'       \item \code{total_tokens}: Total number of tokens used.
#'       \item \code{error}: Error message, if any occurred.
#'     } \cr
#'   \item \code{all_vars}: A vector containing the integrated variable list with every variable generated by the LLM.
#'   \item \code{final_list}: A vector containing the final variable list. If the n_final argument is set in Inf or a number higher than the number or variables in all_vars the final list will be exactly the same as all_vars.
#' }
#'
#' @references \url{https://platform.openai.com}
#'
#' @author Meike Waaijers
#'
#' @note The function and its output should be approached with caution. Depending on the specific LLM used, there may be financial implications. Furthermore, we wish to emphasise that the answers generated by an LLM should not be taken as absolute truth.
#'
#' @seealso
#' \code{\link{cld}},
#' \code{\link{causal_relation}},
#' \code{\link{causal_direction}},
#' \code{\link{causal_sign}},
#' \code{\link{cld_plot}},
#'
#' @examples
#' \dontrun{
#' # For a readily available, pre-made output example see: data("vars")
#' vars <- var_list(context = "addiction",
#'                  n_final = 10,
#'                  n_variables = "all")
#' }
#' # Check output
#' vars$all_vars
#' vars$final_list
#'
#' @import httr
#' @import utils
#' @import keyring
#' @export


## var_list function
var_list <- function(context,
                     include_context = FALSE,
                     n_final = Inf,
                     n_variables = "all",
                     LLM_model = "gpt-4o",
                     max_tokens = 2000,
                     update_key = FALSE) {


  #validate input
  stopifnot("'context' should be a character string." = is.character(context))
  stopifnot("'include_context' should be a logical value." = is.logical(include_context))
  stopifnot("'n_final' should be a whole number above 0." =
              is.numeric(n_final) && n_final == floor(n_final) && n_final >= 0)
  stopifnot("'n_variables' should be a whole number above 0 or the input should be 'all'." =
              ((is.numeric(n_variables) && n_variables == floor(n_variables) && n_variables >= 0) || n_variables == "all"))
  stopifnot("'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', 'mixtral', or 'llama-3'." =
              LLM_model %in% c("mixtral", "gpt-4o", "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo", "llama-3"))
  stopifnot("For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "gpt-4o") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("For 'gpt-4', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "gpt-4") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("For 'gpt-4-turbo', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "gpt-4-turbo") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000." =
              !(LLM_model == "gpt-3.5-turbo") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 3000))
  stopifnot("For 'mixtral', 'max_tokens' should be a whole number above 0, and not higher than 2000." =
              !(LLM_model == "mixtral") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 2000))
  stopifnot("For 'llama-3', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "llama-3") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("'update_key' should be a logical value." = is.logical(update_key))

  ## Load and prepare prompt data
  prompt_file_path <- system.file("extdata", "prompts.csv", package = "theoraizer")
  prompts_data <- utils::read.csv(prompt_file_path, sep = ";")

  # replace '\\n' with '\n' in all text columns
  for (i in 1:length(prompts_data$Prompt)) {
    # Check if the prompt column is a character type
    if (is.character(prompts_data$Prompt[[i]])) {
      # Replace '\\n' with '\n' in the column
      prompts_data$Prompt[[i]] <- gsub("\\n", "\n",
                                       prompts_data$Prompt[[i]],
                                       fixed = TRUE)
    }

    # Check if the sys prompt column is a character type
    if (is.character(prompts_data$Sys.Prompt[[i]])) {
      # Replace '\\n' with '\n' in the column
      prompts_data$Sys.Prompt[[i]] <- gsub("\\n", "\n",
                                           prompts_data$Sys.Prompt[[i]],
                                           fixed = TRUE)
    }
  }

  var_prompts <- prompts_data[prompts_data$Function == "var_list", ]


  ## Create objects for tryCatch output
  # So somethings gets outputted even though an error occurs later on in the function
  raw1_LLM <- NULL
  raw2_LLM <- NULL
  raw3_LLM <- NULL
  logprobs1 <- NULL
  logprobs2 <- NULL
  logprobs3 <- NULL
  raw_LLM <- NULL
  logprobs_out <- NULL
  all_lists_df <- NULL
  variables_f <- NULL


  ## LLM
  runs <- NULL

  # First create 2 prompts
  prompt1 <- gsub("\\((n_variables)\\)", n_variables,
                  gsub("\\((context)\\)", context,
                       var_prompts$Prompt[1]))

  prompt2 <- gsub("\\((n_variables)\\)", n_variables,
                  gsub("\\((context)\\)", context,
                       var_prompts$Prompt[2]))

  prompt_database <- c(prompt1, prompt2)

  # Create system prompts
  system_prompt1 <- gsub("\\((context)\\)", context,
                         var_prompts$Sys.Prompt[1])

  system_prompt2 <- gsub("\\((context)\\)", context,
                         prompts_data$Sys.Prompt[2])

  sys_prompt_database <- c(system_prompt1, system_prompt2)


  for (i in 1:2){
    message(sprintf("Variable list: %d / %d", i, 2))

    prompt <- prompt_database[i]
    system_prompt <- sys_prompt_database[i]

    # LLM
    variable_list <- LLM(prompt = prompt,
                         LLM_model = LLM_model,
                         max_tokens = max_tokens,
                         temperature = 0,
                         logprobs = TRUE,
                         raw_output = TRUE,
                         system_prompt = system_prompt,
                         update_key = update_key)

    update_key <- FALSE # make sure api key is only updated once
    runs[[i]] <- variable_list$output
    raw1_LLM[[i]] <- c(prompt = prompt_database[i], system_prompt = sys_prompt_database[i], variable_list$raw_content)
    logprobs1[[i]] <- variable_list$top5_tokens
  }

  # Clean the output text
  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    split_text <- NULL
    for (k in 1:2){
      split_text[k] <- strsplit(runs[[k]], split = "\n") # Split the string by newline character
    }
    all_lists <- lapply(split_text, function(x) sub("^\\s*[0-9]+\\.\\s+", "", x))
    all_lists <- lapply(all_lists, function(x) trimws(x))

    # Get list lengths
    list_length <- NULL
    for (k in 1:2) {
      list_length[k] <- length(all_lists[[k]]) # make list of list lengths
    }

    # Make all list the same length so they can be put into the same df
    for (l in 1:2) {
      length(all_lists[[l]]) <- max(list_length)
    }

    all_lists_df <- data.frame(all_lists)

    #Change column names
    for (m in 1:2) {
      colnames(all_lists_df)[m] <- paste("List", m, sep = "_")
    }

    #replace na's in with "-"
    all_lists_df[is.na(all_lists_df)] <- "-"

  }, error = function(e){
    cat(paste0("Warning: Unable to process LLM output for list integration -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })


  # Integrating lists
  message("Integrating lists...")

  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    ## LLM for integrating lists
    # collapse all lists into one to put in the prompt
    prompt_list <- paste0(runs, collapse = "\n\n")

    # Create prompt
    prompt_int <- gsub("\\((prompt_list)\\)", prompt_list,
                       var_prompts$Prompt[3])

    # Create system prompt
    system_prompt <- var_prompts$Sys.Prompt[3]

    # LLM
    integrated <- LLM(prompt = prompt_int,
                      LLM_model = LLM_model,
                      max_tokens = max_tokens,
                      temperature = 0,
                      logprobs = TRUE,
                      raw_output = TRUE,
                      system_prompt = system_prompt,
                      update_key = update_key)

    variables_ <- gsub("\n\n", "", integrated$output)  # remove "\n\n"  from LLM output
    raw2_LLM <- c(prompt = prompt_int, system_prompt = system_prompt, integrated$raw_content)
    logprobs2 <- integrated$top5_tokens

  }, error = function(e){
    cat(paste0("Warning: List integration not possible -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })

  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    ## LLM for adding additional variables
    # Create prompt
    prompt_add <- gsub("\\((variables_)\\)", variables_,
                       gsub("\\((context)\\)", context,
                            var_prompts$Prompt[4]))

    # Create system prompt
    system_prompt <- var_prompts$Sys.Prompt[4]

    # LLM
    add_vars <- LLM(prompt = prompt_add,
                    LLM_model = LLM_model,
                    max_tokens = max_tokens,
                    temperature = 0,
                    logprobs = TRUE,
                    raw_output = TRUE,
                    system_prompt = system_prompt,
                    update_key = update_key)

    extra_vars <- gsub("\n\n", "", add_vars$output)  # remove "\n\n"  from LLM output
    raw3_LLM <- c(prompt = prompt_add, system_prompt = system_prompt, add_vars$raw_content)
    logprobs3 <- add_vars$top5_tokens

  }, error = function(e){
    cat(paste0("Warning: Unable to process LLM output for adding missing variables -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })


  ## Clean up variable list
  message("Cleaning up integrated list...")

  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    ## LLM for integrating lists
    # Create prompt
    prompt_clean <- gsub("\\((extra_vars)\\)", extra_vars,
                         var_prompts$Prompt[5])

    # Create system prompt
    system_prompt <- gsub("\\((context)\\)", context,
                          var_prompts$Sys.Prompt[5])

    # LLM
    clean <- LLM(prompt = prompt_clean,
                 LLM_model = LLM_model,
                 max_tokens = max_tokens,
                 temperature = 0,
                 logprobs = TRUE,
                 raw_output = TRUE,
                 system_prompt = system_prompt,
                 update_key = update_key)

    cleaned_vars <- gsub("\n\n", "", clean$output)  # remove "\n\n"  from LLM output
    raw4_LLM <- c(prompt = prompt_clean, system_prompt = system_prompt, clean$raw_content)
    logprobs4 <- clean$top5_tokens

  }, error = function(e){
    cat(paste0("Warning: List integration not possible -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })


  tryCatch({

    #Clean the output text
    split_text <- strsplit(cleaned_vars, split = "\n") # Split the string by newline character
    variables <- lapply(split_text, function(x) sub("^\\s*[0-9]+\\.\\s+", "", x))
    variables <- lapply(variables, function(x) trimws(x))


  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })

  tryCatch({
    ## LLM for limiting the final list to a list with the most ... important variables
    if (length(variables[[1]]) > n_final) {

      message(sprintf("Selecting the %d most important variables...", n_final))

      #tryCatch in case processing steps fail the raw output will still be outputted
      tryCatch({

        # Create prompt
        prompt_limit_int <- gsub("\\((cleaned_vars)\\)", cleaned_vars,
                                 gsub("\\((n_final)\\)", n_final,
                                      gsub("\\((context)\\)", context,
                                           var_prompts$Prompt[6])))

        # Create system prompt
        system_prompt <- var_prompts$Sys.Prompt[6]

        # LLM
        limit_integrated <- LLM(prompt = prompt_limit_int,
                                LLM_model = LLM_model,
                                max_tokens = max_tokens,
                                temperature = 0,
                                logprobs = TRUE,
                                raw_output = TRUE,
                                system_prompt = system_prompt,
                                update_key = update_key)

        variables_imp <- gsub("\n\n", " ", limit_integrated$output)  # remove "\n\n"  from LLM output
        raw5_LLM <- c(prompt = prompt_limit_int, system_prompt = system_prompt, limit_integrated$raw_content)
        logprobs5 <- limit_integrated$top5_tokens

      }, error = function(e){
        cat(paste0("Warning: Limiting integrated list not possible -> ", e$message, "."),
            "Only part of the output is returned.", sep = "\n")
      })

      #tryCatch in case processing steps fail the raw output will still be outputted
      tryCatch({
        #Clean the output text
        split_text <- strsplit(variables_imp, split = "\n") # Split the string by newline character
        variables_f <- lapply(split_text, function(x) sub("^\\s*[0-9]+\\.\\s+", "", x))
        variables_f <- lapply(variables_f, function(x) trimws(x))

      }, error = function(e) {
        cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
            "Only part of the output is returned.", sep = "\n")
      })

    } else {
      variables_f <- variables
      limit_integrated <- NULL
      raw5_LLM <- NULL
      logprobs5 <- NULL
    }

  }, error = function(e){
    cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })


  # Initialize the output list
  output <- list()
  flattened_df_raw1_LLM <- NULL
  flattened_df_raw2_LLM <- NULL
  flattened_df_raw3_LLM <- NULL
  flattened_df_raw4_LLM <- NULL
  flattened_df_raw5_LLM <- NULL


  # Add raw_LLM to output
  tryCatch({
    # Initialize empty dataframe
    flattened_df_raw1_LLM <- data.frame(function_part = character(),
                                        iteration = integer(),
                                        LLM_model = character(),
                                        prompt = character(),
                                        system_prompt = character(),
                                        content = character(),
                                        finish_reason = character(),
                                        prompt_tokens = numeric(),
                                        answer_tokens = numeric(),
                                        total_tokens = numeric(),
                                        error = character(),
                                        stringsAsFactors = FALSE)

    # Flatten raw1
    for (i in seq_along(raw1_LLM)) {
      temp <- raw1_LLM[[i]]
      flattened_df_raw1_LLM <- rbind(flattened_df_raw1_LLM,
                                     data.frame(function_part = "create_lists",
                                                iteration = i,
                                                LLM_model = temp$LLM_model,
                                                prompt = temp$prompt,
                                                system_prompt = temp$system_prompt,
                                                content = temp$content,
                                                finish_reason = temp$finish_reason,
                                                prompt_tokens = temp$prompt_tokens,
                                                answer_tokens = temp$answer_tokens,
                                                total_tokens = temp$total_tokens,
                                                error = ifelse(is.null(temp$error), NA, temp$error),
                                                stringsAsFactors = FALSE))
    }

    # Flatten raw2
    flattened_df_raw2_LLM <- data.frame(function_part = "integrate_lists",
                                        iteration = 1,
                                        LLM_model = raw2_LLM$LLM_model,
                                        prompt = raw2_LLM$prompt,
                                        system_prompt = raw2_LLM$system_prompt,
                                        content = raw2_LLM$content,
                                        finish_reason = raw2_LLM$finish_reason,
                                        prompt_tokens = raw2_LLM$prompt_tokens,
                                        answer_tokens = raw2_LLM$answer_tokens,
                                        total_tokens = raw2_LLM$total_tokens,
                                        error = ifelse(is.null(raw2_LLM$error), NA, raw2_LLM$error),
                                        stringsAsFactors = FALSE)

    # Flatten raw3
    flattened_df_raw3_LLM <- data.frame(function_part = "add_vars",
                                        iteration = 1,
                                        LLM_model = raw3_LLM$LLM_model,
                                        prompt = raw3_LLM$prompt,
                                        system_prompt = raw3_LLM$system_prompt,
                                        content = raw3_LLM$content,
                                        finish_reason = raw3_LLM$finish_reason,
                                        prompt_tokens = raw3_LLM$prompt_tokens,
                                        answer_tokens = raw3_LLM$answer_tokens,
                                        total_tokens = raw3_LLM$total_tokens,
                                        error = ifelse(is.null(raw3_LLM$error), NA, raw3_LLM$error),
                                        stringsAsFactors = FALSE)

    # Flatten raw4
    flattened_df_raw4_LLM <- data.frame(function_part = "clean_list",
                                        iteration = 1,
                                        LLM_model = raw4_LLM$LLM_model,
                                        prompt = raw4_LLM$prompt,
                                        system_prompt = raw4_LLM$system_prompt,
                                        content = raw4_LLM$content,
                                        finish_reason = raw4_LLM$finish_reason,
                                        prompt_tokens = raw4_LLM$prompt_tokens,
                                        answer_tokens = raw4_LLM$answer_tokens,
                                        total_tokens = raw4_LLM$total_tokens,
                                        error = ifelse(is.null(raw4_LLM$error), NA, raw4_LLM$error),
                                        stringsAsFactors = FALSE)


    if (!is.null(raw5_LLM)) {
      flattened_df_raw5_LLM <- data.frame(function_part = "limit_list",
                                          iteration = 1,
                                          LLM_model = raw5_LLM$LLM_model,
                                          prompt = raw5_LLM$prompt,
                                          system_prompt = raw5_LLM$system_prompt,
                                          content = raw5_LLM$content,
                                          finish_reason = raw5_LLM$finish_reason,
                                          prompt_tokens = raw5_LLM$prompt_tokens,
                                          answer_tokens = raw5_LLM$answer_tokens,
                                          total_tokens = raw5_LLM$total_tokens,
                                          error = ifelse(is.null(raw5_LLM$error), NA, raw5_LLM$error),
                                          stringsAsFactors = FALSE)

    } else {
      flattened_df_raw5_LLM <- data.frame(function_part = "limit_list",
                                          iteration = 0,
                                          LLM_model = NA,
                                          prompt = NA,
                                          system_prompt = NA,
                                          content = NA,
                                          finish_reason = NA,
                                          prompt_tokens = NA,
                                          answer_tokens = NA,
                                          total_tokens = NA,
                                          error = NA,
                                          stringsAsFactors = FALSE)
    }

    # Combine the five dataframes
    raw_LLM_df <- rbind(flattened_df_raw1_LLM,
                        flattened_df_raw2_LLM,
                        flattened_df_raw3_LLM,
                        flattened_df_raw4_LLM,
                        flattened_df_raw5_LLM)
    output$raw_LLM <- raw_LLM_df

  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  tryCatch({
    # Add cleaned list to output
    output$all_vars <- variables[[1]]

    # Add final_list to output
    output$final_list <- variables_f[[1]]

    # Include context if include_context = TRUE
    if (include_context == TRUE) {
      output$final_list <- c(output$final_list, context)
    }

    if (length(variables[[1]]) > n_final) {
      message("Total of LLM prompts: 6")

    } else {
      message("Total of LLM prompts: 5")

    }
  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  # give openai error if there is no output at all
  if (length(output) == 0) {
    if (!is.null(raw1_LLM[[1]]$error$message)) {
      stop(raw1_LLM[[1]]$error$message)
    } else if (!is.null(raw1_LLM[[2]]$error$message)) {
      stop(raw1_LLM[[2]]$error$message)
    } else if (!is.null(raw2_LLM$error$message)) {
      stop(raw2_LLM$error$message)
    } else if (!is.null(raw3_LLM$error$message)) {
      stop(raw3_LLM$error$message)
    } else if (!is.null(raw4_LLM$error$message)) {
      stop(raw4_LLM$error$message)
    } else if (!is.null(raw5_LLM$error$message)) {
      stop(raw5_LLM$error$message)
    }
  }

  return(output)
}
