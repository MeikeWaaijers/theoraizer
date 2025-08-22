# Program Name: theoraizer
# Description: In the causal_sign function a Large Language Model (LLM) is asked to determine whether a causal relationship is positive or negative.
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
### Sign of causal relationship (positive or negative relationship)

## Function manual

#' Identify the Sign of Causal Relationships
#'
#' @description
#' In \code{causal_sign()} a Large Language Model (LLM) is asked to determine whether a causal relationship is positive or negative. This is achieved by asking the LLM how an increase or decrease in one variable affects the other variable.
#'
#' @usage
#' causal_sign(topic,
#'             prob_df,
#'             causal_threshold = 50,
#'             LLM_model = "gpt-4o",
#'             max_tokens = 2000,
#'             update_key = FALSE)
#'
#' @details
#' To create a theory from scratch, the functions in this R-package should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}} --> \code{\link{find_source}}
#'
#' @param topic A character vector specifying the topic for which a theory should be developed. If it is not feasible to identify a particular topic, the argument can be set to NULL.
#' @param prob_df Two different probability dataframes can be inputted:
#' \itemize{
#'   \item A dataframe with 3 columns and on every row a unique variable pair and the probability of the presence of a causal relationship between these variables (The \code{relation_df} output from the \code{\link{causal_relation}} function).
#'   \item A dataframe with 5 columns and on every row a unique variable pair, the probability of the presence of a causal relationship between these variables, and the cause variable probability for each variable in the pair (The \code{direction_df} output from the \code{\link{causal_direction}} function).
#' }
#' @param causal_threshold A number (defaults to \code{50}) that indicates the minimum probability required for a causal relationship to be included in the plot.
#' @inheritParams var_list
#'
#' @returns
#' The output is an extended version of the inputted probability dataframe. The output will therefore vary depending on whether the original input was a relation or a direction dataframe.
#'
#' \itemize{
#'  If a relation dataframe is inputted:
#'   \itemize{
#'     \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'       \itemize{
#'         \item \code{relationship}: Which variable pair.
#'         \item \code{iteration}: Iteration number.
#'         \item \code{LLM_model}: LLM model used.
#'         \item \code{prompt}: Prompt used.
#'         \item \code{system_prompt}: System prompt used.
#'         \item \code{content}: Unprocessed LLM output.
#'         \item \code{finish_reason}: Reason the LLM stopped generating output.
#'         \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
#'         \item \code{answer_tokens}: Number of tokens used for the LLM answer.
#'         \item \code{total_tokens}: Total number of tokens used.
#'         \item \code{error}: Error message, if any occurred.
#'     }
#'   } \cr
#'   \itemize{
#'     \item \code{sign_df}: A dataframe with five columns:
#'       \itemize{
#'         \item \code{var1}: Variable 1 of a unique variable pair.
#'         \item \code{var2}: Variable 2 of a unique variable pair.
#'         \item \code{prob_causal}: Probability of the presence of a causal relationship between var1 and var2.
#'         \item \code{prob_pos}: Probability of a positive relationship.
#'         \item \code{prob_neg}: Probability of a negative relationship.
#'       }
#'   }
#' } \cr
#' \itemize{
#'  If a direction dataframe is inputted:
#'   \itemize{
#'     \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'       \itemize{
#'         \item \code{var}: Which variable is put in the prompt as cause variable.
#'         \item \code{relationship}: Which variable pair.
#'         \item \code{iteration}: Iteration number.
#'         \item \code{LLM_model}: LLM model used.
#'         \item \code{prompt}: Prompt used.
#'         \item \code{system_prompt}: System prompt used.
#'         \item \code{content}: Unprocessed LLM output.
#'         \item \code{finish_reason}: Reason the LLM stopped generating output.
#'         \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
#'         \item \code{answer_tokens}: Number of tokens used for the LLM answer.
#'         \item \code{total_tokens}: Total number of tokens used.
#'         \item \code{error}: Error message, if any occurred.
#'     }
#'   } \cr
#'   \itemize{
#'     \item \code{sign_df}: A dataframe with nine columns:
#'       \itemize{
#'         \item \code{var1}: Variable 1 of a unique variable pair.
#'         \item \code{var2}: Variable 2 of a unique variable pair.
#'         \item \code{prob_causal}: Probability of the presence of a causal relationship between var1 and var2.
#'         \item \code{prob_var1_cause}: Probability of var1 being a cause variable.
#'         \item \code{prob_var1_pos}: Probability of a positive relationship where variable 1 is the cause.
#'         \item \code{prob_var1_neg}: Probability of a negative relationship where variable 1 is the cause.
#'         \item \code{prob_var2_cause}: Probability of var2 being a cause variable.
#'         \item \code{prob_var2_pos}: Probability of a positive relationship where variable 2 is the cause.
#'         \item \code{prob_var2_neg}: Probability of a negative relationship where variable 2 is the cause.
#'     }
#'   }
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
#' \code{\link{var_list}},
#' \code{\link{causal_relation}},
#' \code{\link{causal_direction}},
#' \code{\link{cld_plot}},
#' \code{\link{find_source}}
#'
#' @examples
#' \dontrun{
#' ## Example input (topic = "addiction")
#' # Relation probability dataframe input
#' data("rel")
#' rel$relation_df
#'
#' # Direction probability dataframe input
#' data("dir")
#' dir$direction_df
#'
#' #---------------------------------------------------------------------------
#' ## Create sign dataframe for a relation probability dataframe
#' # For a readily available, pre-made output example see: data("rel_sign")
#' rel_sign <- causal_sign(topic = "addiction",
#'                         prob_df = rel$relation_df)
#'
#' # Check output
#' rel_sign$sign_df
#'
#' #---------------------------------------------------------------------------
#' ## Create sign dataframe for a direction probability dataframe
#' # For a readily available, pre-made output example see: data("dir_sign")
#' dir_sign <- causal_sign(topic = "addiction",
#'                         prob_df = dir$direction_df)
#'
#' # Check output
#' dir_sign$sign_df
#' }
#'
#' @import httr
#' @import utils
#' @import keyring
#' @export


## causal_sign function
causal_sign <- function(topic,
                        prob_df,
                        causal_threshold = 50,
                        LLM_model = "gpt-4o",
                        max_tokens = 2000,
                        update_key = FALSE) {

  #validate input
  stopifnot(
    "'topic' should be a single non-empty character string or NULL." =
      is.null(topic) ||
      (is.character(topic) && length(topic) == 1L && !is.na(topic) && nzchar(trimws(topic)))
  )
  stopifnot("'causal_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
              is.numeric(causal_threshold) && causal_threshold >= 0 && causal_threshold <= 100 && round(causal_threshold, 2) == causal_threshold)
  stopifnot("'prob_df' should be a dataframe." = is.data.frame(prob_df))

  if (ncol(prob_df) == 3) {
    which_df <- "relation"
  } else if (ncol(prob_df) == 5) {
    which_df <- "direction"
  } else {
    which_df <- 0
  }

  stopifnot("'prob_df' should be either a 'relation_df' or a 'direction_df'." =
              which_df == "relation" | which_df == "direction")

  if (which_df == "relation"){
    stopifnot("'prob_df' should have three columns named 'var1', 'var2' and 'prob_causal'." =
                ncol(prob_df) == 3 && all(paste(c('var1', 'var2', 'prob_causal'), collapse = ", ") == paste(names(prob_df), collapse = ", ")))

  } else if (which_df == "direction") {
    stopifnot("'prob_df' should have five columns named 'var1', 'var2', 'prob_causal', 'prob_var1_cause', 'prob_var2_cause'." =
                ncol(prob_df) == 5 && all(paste(c('var1', 'var2', 'prob_causal', 'prob_var1_cause', 'prob_var2_cause'), collapse = ", ") == paste(names(prob_df), collapse = ", ")))
    stopifnot("All entries in 'prob_df$prob_var1_cause' and 'prob_df$prob_var2_cause' should be numeric and between 0 and 100." =
                all(sapply(prob_df$prob_var1_cause, function(x) is.numeric(x) && x >= 0 && x <= 100)) &&
                all(sapply(prob_df$prob_var2_cause, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("At least one variable should be classified as a 'cause variable'." = sum(prob_df[,c(4,5)]) > 0)
  }

  stopifnot("All entries in 'prob_df$var1' and 'prob_df$var2' should be character strings." =
              all(sapply(prob_df$var1, is.character)) && all(sapply(prob_df$var2, is.character)))
  stopifnot("All entries in 'prob_df$prob_causal' should be numeric and between 0 and 100." =
              all(sapply(prob_df$prob_causal, function(x) is.numeric(x) && x >= 0 && x <= 100)))
  stopifnot("At least one variable pair should be classified as 'causal'." = sum(prob_df$prob_causal) > 0)
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
      prompts_data$Prompt[[i]] <- gsub("\\n", "\n", prompts_data$Prompt[[i]], fixed = TRUE)
    }

    # Check if the sys prompt column is a character type
    if (is.character(prompts_data$Sys.Prompt[[i]])) {
      # Replace '\\n' with '\n' in the column
      prompts_data$Sys.Prompt[[i]] <- gsub("\\n", "\n", prompts_data$Sys.Prompt[[i]], fixed = TRUE)
    }
  }

  sign_prompts <- prompts_data[prompts_data$Function == "sign", ]

  ## Get names of the variables
  var_1 <- prob_df$var1
  var_2 <- prob_df$var2


  ## Relation df input
  if(which_df == "relation") {
    row_index <- unique(which(prob_df[3] > causal_threshold, arr.ind = TRUE)[, 1]) # get row_index of all first variables indicated to be a cause at least once

    ## Create objects for tryCatch output
    # So somethings gets outputted even though an error occurs later on in the function
    raw_posneg <- NULL
    raw_LLM <- NULL
    logprobs_posneg <- NULL
    logprobs_LLM <- NULL
    sign_df <- NULL

    ## LLM
    pair <- 1

    for (b in row_index) {
      message(sprintf("Variable pair: %d / %d", pair, length(row_index)))

      # Initialize the prompt database outside of the loops
      prompt_database <- list()
      system_prompt_database <- list()

      for (c in 1:8) {
        if (length(prompt_database) == 0) {
          if (is.null(topic)){
            # Create 8 prompts
            prompt1 <- gsub("\\((var_1\\[b\\])\\)", var_1[b],
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 sign_prompts$Prompt[1]))
            prompt2 <- gsub("\\((var_1\\[b\\])\\)", var_1[b],
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 sign_prompts$Prompt[2]))
            prompt3 <- gsub("\\((var_1\\[b\\])\\)", var_1[b],
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 sign_prompts$Prompt[3]))
            prompt4 <- gsub("\\((var_1\\[b\\])\\)", var_1[b],
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 sign_prompts$Prompt[4]))
            prompt5 <- gsub("\\((var_2\\[b\\])\\)", var_2[b],
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 sign_prompts$Prompt[5]))
            prompt6 <- gsub("\\((var_2\\[b\\])\\)", var_2[b],
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 sign_prompts$Prompt[6]))
            prompt7 <- gsub("\\((var_2\\[b\\])\\)", var_2[b],
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 sign_prompts$Prompt[7]))
            prompt8 <- gsub("\\((var_2\\[b\\])\\)", var_2[b],
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 sign_prompts$Prompt[8]))

          } else {
            # Create 8 prompts
            prompt1 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                      sign_prompts$Prompt[9])))
            prompt2 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                      sign_prompts$Prompt[10])))
            prompt3 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                      sign_prompts$Prompt[11])))
            prompt4 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                 gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                      sign_prompts$Prompt[12])))
            prompt5 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                      sign_prompts$Prompt[13])))
            prompt6 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                      sign_prompts$Prompt[14])))
            prompt7 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                      sign_prompts$Prompt[15])))
            prompt8 <- gsub("\\((topic)\\)", topic,
                            gsub("\\((var_2\\[b\\])\\)", var_2[b],
                                 gsub("\\((var_1\\[b\\])\\)", var_1[b],
                                      sign_prompts$Prompt[16])))

          }

          # Put all prompts in a database
          prompt_database <- list(prompt1, prompt2, prompt3, prompt4,
                                  prompt5, prompt6, prompt7, prompt8)
        }

        prompt <- prompt_database[[c]]

        if (is.null(topic)) {
          # Create 2 system prompts
          system_prompt1 <- sign_prompts$Sys.Prompt[1]
          system_prompt2 <- sign_prompts$Sys.Prompt[2]

        } else {
          # Create 2 system prompts
          system_prompt1 <- sign_prompts$Sys.Prompt[3]
          system_prompt2 <- sign_prompts$Sys.Prompt[4]

        }

        if (length(system_prompt_database) == 0) {
          # Put system prompts in a database
          system_prompt_database <- list(system_prompt1,
                                         system_prompt2,
                                         system_prompt1,
                                         system_prompt2,
                                         system_prompt1,
                                         system_prompt2,
                                         system_prompt1,
                                         system_prompt2)
        }

        system_prompt <- system_prompt_database[[c]]

        # LLM
        pos_neg <- LLM(prompt = prompt,
                       LLM_model = LLM_model,
                       max_tokens = ifelse(LLM_model == "mixtral", 4, max_tokens),
                       temperature = 0,
                       logprobs = TRUE,
                       raw_output = TRUE,
                       system_prompt = system_prompt,
                       update_key = update_key)

        update_key <- FALSE # make sure api key is only updated once
        raw_posneg[c] <- list(c(prompt = prompt, system_prompt = system_prompt, pos_neg$raw_content))
        logprobs_posneg[c] <- list(pos_neg$top5_tokens)

      }

      raw_LLM[[b]] <- raw_posneg
      logprobs_LLM[[b]] <- logprobs_posneg

      pair <- pair + 1
    }

    #tryCatch in case processing steps fail the raw output will still be outputted
    tryCatch({
      if (LLM_model == "mixtral" | LLM_model == "llama-3") {
        last_token_f <- NULL
        for (i in row_index) {
          last_token_t <- NULL
          for (j in 1:length(logprobs_LLM[[i]])){
            last_token_t[[j]] <- logprobs_LLM[[i]][[j]][[1]]
            last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
          }
          last_token_f[[i]] <- last_token_t
        }

      } else {
        last_token_f <- NULL
        for (i in row_index) {
          last_token_t <- NULL
          for (j in 1:length(logprobs_LLM[[i]])){
            last_token_t[[j]] <- logprobs_LLM[[i]][[j]][[length(logprobs_LLM[[i]][[j]])]]
            last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
          }
          last_token_f[[i]] <- last_token_t
        }
      }

      all_prob <- list()
      valid_tokens <- c("in", "de")

      for (l in row_index) {
        probs <- list()
        for (g in 1:length(last_token_f[[l]])){
          class <- NULL
          prob <- NULL
          k <- 1
          # Process each item in last_token
          for (m in 1:nrow(last_token_f[[l]][[g]])) {
            if (trimws(tolower(last_token_f[[l]][[g]]$top5_tokens[m])) %in% valid_tokens) {
              # Filter and extract required values
              if (g == 1 | g == 2 | g == 5 | g == 6){
                if (trimws(tolower(gsub("[\n]", "", last_token_f[[l]][[g]]$top5_tokens[m]))) == "in"){
                  class[k] <- "positive"
                } else if (trimws(tolower(gsub("[\n]", "", last_token_f[[l]][[g]]$top5_tokens[m]))) == "de"){
                  class[k] <- "negative"
                }
              } else if (g == 3 | g == 4 | g == 7 | g == 8){
                if (trimws(tolower(gsub("[\n]", "", last_token_f[[l]][[g]]$top5_tokens[m]))) == "in"){
                  class[k] <- "negative"
                } else if (trimws(tolower(gsub("[\n]", "", last_token_f[[l]][[g]]$top5_tokens[m]))) == "de"){
                  class[k] <- "positive"
                }
              }
              prob[k] <- last_token_f[[l]][[g]]$probability[m]
              k <- k + 1
            }
          }
          # Filter positive probabilities
          positive_probs <- prob > 0
          probs[[g]] <- data.frame(Class = class[positive_probs], Probability = prob[positive_probs])
        }
        all_prob[[l]] <- probs
      }


      ## Certainty of sign per causal relationship
      pos_prob_f <- NULL
      neg_prob_f <- NULL
      for (w in row_index) {
        pos_prob_t <- 0
        neg_prob_t <- 0
        for (z in 1:8) {
          for (x in 1:nrow(all_prob[[w]][[z]])) {
            if (all_prob[[w]][[z]]$Class[[x]] == "positive") {
              pos_prob_t <- pos_prob_t + all_prob[[w]][[z]]$Probability[[x]]
            } else if (all_prob[[w]][[z]]$Class[[x]] == "negative") {
              neg_prob_t <- neg_prob_t + all_prob[[w]][[z]]$Probability[[x]]
            }
          }
        }
        pos_prob_f[w] <- ifelse((pos_prob_t / 8) > 100, round((pos_prob_t / 8)), round((pos_prob_t / 8), 2))
        neg_prob_f[w] <- ifelse((neg_prob_t / 8) > 100, round((neg_prob_t / 8)), round((neg_prob_t / 8), 2))
      }

      #replace absent relations with 0
      pos_prob_f[is.na(pos_prob_f)] <- 0
      neg_prob_f[is.na(neg_prob_f)] <- 0

      #create sign df
      sign_df <- prob_df
      #make sure dataframes are of equal length and add sign to sign_df
      if(length(pos_prob_f) < nrow(sign_df)) {
        pos_prob_f[(length(pos_prob_f) + 1) : nrow(sign_df)] <- 0
      }
      if(length(neg_prob_f) < nrow(sign_df)) {
        neg_prob_f[(length(neg_prob_f) + 1) : nrow(sign_df)] <- 0
      }
      sign_df$prob_pos <- pos_prob_f
      sign_df$prob_neg <- neg_prob_f

      # rearrange columns
      sign_df <- sign_df[, c("var1",
                             "var2",
                             "prob_causal",
                             "prob_pos",
                             "prob_neg")]

    }, error = function(e) {
      cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
          "Only part of the output is returned.", sep = "\n")
    })


    ## Direction df input
  } else if (which_df == "direction") {

    row_index_var1 <- unique(which(prob_df[4] > causal_threshold, arr.ind = TRUE)[, 1]) # get row_index of all first variables indicated to be a cause at least once
    row_index_var2 <- unique(which(prob_df[5] > causal_threshold, arr.ind = TRUE)[, 1]) # get row_index of all second variables indicated to be a cause at least once

    ## Create objects for tryCatch output
    # So somethings gets outputted even though an error occurs later on in the function
    raw_posneg_var1 <- NULL
    raw_LLM_var1 <- NULL
    logprobs_posneg_var1 <- NULL
    logprobs_LLM_var1 <- NULL
    raw_posneg_var2 <- NULL
    raw_LLM_var2 <- NULL
    logprobs_posneg_var2 <- NULL
    logprobs_LLM_var2 <- NULL
    sign_df <- NULL

    ## LLM var 1
    pair <- 1

    print("1/2")
    for (i in row_index_var1) {
      print(paste("Variable pair:  ", pair,  "/", length(row_index_var1)))

      # Initialize the prompt database outside of the loops
      var1_prompt_database <- list()
      var1_system_prompt_database <- list()

      for (g in 1:4) {
        if (length(var1_prompt_database) == 0) {
          if (is.null(topic)) {
            # Create 8 prompts for var 1
            var1_prompt1 <- gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                 gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                      sign_prompts$Prompt[17]))
            var1_prompt2 <- gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                 gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                      sign_prompts$Prompt[18]))
            var1_prompt3 <- gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                 gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                      sign_prompts$Prompt[19]))
            var1_prompt4 <- gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                 gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                      sign_prompts$Prompt[20]))

          } else {
            # Create 8 prompts for var 1
            var1_prompt1 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                      gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                           sign_prompts$Prompt[21])))
            var1_prompt2 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                      gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                           sign_prompts$Prompt[22])))
            var1_prompt3 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                      gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                           sign_prompts$Prompt[23])))
            var1_prompt4 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_1\\[i\\])\\)", var_1[i],
                                      gsub("\\((var_2\\[i\\])\\)", var_2[i],
                                           sign_prompts$Prompt[24])))

          }

          # Put all prompts in a database
          var1_prompt_database <- list(var1_prompt1,
                                       var1_prompt2,
                                       var1_prompt3,
                                       var1_prompt4)

        }

        var1_prompt <- var1_prompt_database[[g]]


        if (is.null(topic)) {
          # Create 2 system prompts
          system_prompt1 <- sign_prompts$Sys.Prompt[1]
          system_prompt2 <- sign_prompts$Sys.Prompt[2]

        } else {
          # Create 2 system prompts
          system_prompt1 <- sign_prompts$Sys.Prompt[3]
          system_prompt2 <- sign_prompts$Sys.Prompt[4]

        }

        if (length(var1_system_prompt_database) == 0) {
          # Put system prompts in a database
          var1_system_prompt_database <- list(system_prompt1,
                                              system_prompt2,
                                              system_prompt1,
                                              system_prompt2)
        }

        var1_system_prompt <- var1_system_prompt_database[[g]]


        # LLM
        var1_pos_neg <- LLM(prompt = var1_prompt,
                            LLM_model = LLM_model,
                            max_tokens = ifelse(LLM_model == "mixtral", 4, max_tokens),
                            temperature = 0,
                            logprobs = TRUE,
                            raw_output = TRUE,
                            system_prompt = var1_system_prompt,
                            update_key = update_key)

        update_key <- FALSE # make sure api key is only updated once
        raw_posneg_var1[g] <- list(c(prompt = var1_prompt, system_prompt = var1_system_prompt, var1_pos_neg$raw_content))
        logprobs_posneg_var1[g] <- list(var1_pos_neg$top5_tokens)

      }
      raw_LLM_var1[[i]] <- raw_posneg_var1
      logprobs_LLM_var1[[i]] <- logprobs_posneg_var1

      pair <- pair + 1
    }


    #LLM var 2
    pair <- 1

    print("2/2")
    for (l in row_index_var2) {
      print(paste("Variable pair:  ", pair,  "/", length(row_index_var2)))

      # Initialize the prompt database outside of the loops
      var2_prompt_database <- list()
      var2_system_prompt_database <- list()

      for (k in 1:4) {
        if (length(var2_prompt_database) == 0) {
          if (is.null(topic)) {
            # Create 4 prompts for var 2
            var2_prompt1 <- gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                 gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                      sign_prompts$Prompt[25]))
            var2_prompt2 <- gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                 gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                      sign_prompts$Prompt[26]))
            var2_prompt3 <- gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                 gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                      sign_prompts$Prompt[27]))
            var2_prompt4 <- gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                 gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                      sign_prompts$Prompt[28]))


          } else {
            # Create 4 prompts for var 2
            var2_prompt1 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                      gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                           sign_prompts$Prompt[29])))
            var2_prompt2 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                      gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                           sign_prompts$Prompt[30])))
            var2_prompt3 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                      gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                           sign_prompts$Prompt[31])))
            var2_prompt4 <- gsub("\\((topic)\\)", topic,
                                 gsub("\\((var_2\\[l\\])\\)", var_2[l],
                                      gsub("\\((var_1\\[l\\])\\)", var_1[l],
                                           sign_prompts$Prompt[32])))
          }

          # Put all prompts in a database
          var2_prompt_database <- list(var2_prompt1,
                                       var2_prompt2,
                                       var2_prompt3,
                                       var2_prompt4)

        }

        var2_prompt <- var2_prompt_database[[k]]


        if (is.null(topic)) {
          # Create 2 system prompts
          system_prompt1 <- sign_prompts$Sys.Prompt[1]
          system_prompt2 <- sign_prompts$Sys.Prompt[2]

        } else {
          # Create 2 system prompts
          system_prompt1 <- sign_prompts$Sys.Prompt[3]
          system_prompt2 <- sign_prompts$Sys.Prompt[4]

        }

        if (length(var2_system_prompt_database) == 0) {
          # Put system prompts in a database
          var2_system_prompt_database <- list(system_prompt1,
                                              system_prompt2,
                                              system_prompt1,
                                              system_prompt2)
        }

        var2_system_prompt <- var2_system_prompt_database[[k]]


        # LLM
        var2_pos_neg <- LLM(prompt = var2_prompt,
                            LLM_model = LLM_model,
                            max_tokens = ifelse(LLM_model == "mixtral", 4, max_tokens),
                            temperature = 0,
                            logprobs = TRUE,
                            raw_output = TRUE,
                            system_prompt = var2_system_prompt,
                            update_key = update_key)

        update_key <- FALSE # make sure api key is only updated once
        raw_posneg_var2[k] <- list(c(prompt = var2_prompt, system_prompt = var2_system_prompt, var2_pos_neg$raw_content))
        logprobs_posneg_var2[k] <- list(var2_pos_neg$top5_tokens)

      }

      raw_LLM_var2[[l]] <- raw_posneg_var2
      logprobs_LLM_var2[[l]] <- logprobs_posneg_var2

      pair <- pair + 1
    }


    ## Make output LLM ready for function output
    # Var 1

    #tryCatch in case processing steps fail the raw output will still be outputted
    tryCatch({
      if (LLM_model == "mixtral" | LLM_model == "llama-3"){
        last_token_f_var1 <- NULL
        for (i in row_index_var1) {
          last_token_t <- NULL
          for (j in 1:length(logprobs_LLM_var1[[i]])){
            last_token_t[[j]] <- logprobs_LLM_var1[[i]][[j]][[1]]
            last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
          }
          last_token_f_var1[[i]] <- last_token_t
        }

      } else {
        last_token_f_var1 <- NULL
        for (i in row_index_var1) {
          last_token_t <- NULL
          for (j in 1:length(logprobs_LLM_var1[[i]])){
            last_token_t[[j]] <- logprobs_LLM_var1[[i]][[j]][[length(logprobs_LLM_var1[[i]][[j]])]]
            last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
          }
          last_token_f_var1[[i]] <- last_token_t
        }
      }

      all_prob_var1 <- list()
      valid_tokens <- c("in", "de")

      for (l in row_index_var1) {
        probs <- list()
        for (g in 1:length(last_token_f_var1[[l]])){
          class <- NULL
          prob <- NULL
          k <- 1
          # Process each item in last_token
          for (m in 1:nrow(last_token_f_var1[[l]][[g]])) {
            if (trimws(tolower(last_token_f_var1[[l]][[g]]$top5_tokens[m])) %in% valid_tokens) {
              # Filter and extract required values
              if (g == 1 | g == 2){
                if (trimws(tolower(gsub("[\n]", "", last_token_f_var1[[l]][[g]]$top5_tokens[m]))) == "in"){
                  class[k] <- "positive"
                } else if (trimws(tolower(gsub("[\n]", "", last_token_f_var1[[l]][[g]]$top5_tokens[m]))) == "de"){
                  class[k] <- "negative"
                }
              } else if (g == 3 | g == 4){
                if (trimws(tolower(gsub("[\n]", "", last_token_f_var1[[l]][[g]]$top5_tokens[m]))) == "in"){
                  class[k] <- "negative"
                } else if (trimws(tolower(gsub("[\n]", "", last_token_f_var1[[l]][[g]]$top5_tokens[m]))) == "de"){
                  class[k] <- "positive"
                }
              }

              prob[k] <- last_token_f_var1[[l]][[g]]$probability[m]
              k <- k + 1
            }
          }
          # Filter positive probabilities
          positive_probs <- prob > 0
          probs[[g]] <- data.frame(Class = class[positive_probs], Probability = prob[positive_probs])
        }
        all_prob_var1[[l]] <- probs
      }


      # var2
      if (LLM_model == "mixtral" | LLM_model == "llama-3"){
        last_token_f_var2 <- NULL
        for (i in row_index_var2) {
          last_token_t <- NULL
          for (j in 1:length(logprobs_LLM_var2[[i]])){
            last_token_t[[j]] <- logprobs_LLM_var2[[i]][[j]][[1]]
            last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
          }
          last_token_f_var2[[i]] <- last_token_t
        }

      } else {
        last_token_f_var2 <- NULL
        for (i in row_index_var2) {
          last_token_t <- NULL
          for (j in 1:length(logprobs_LLM_var2[[i]])){
            last_token_t[[j]] <- logprobs_LLM_var2[[i]][[j]][[length(logprobs_LLM_var2[[i]][[j]])]]
            last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
          }
          last_token_f_var2[[i]] <- last_token_t
        }
      }


      all_prob_var2 <- list()

      for (l in row_index_var2) {
        probs <- list()
        for (g in 1:length(last_token_f_var2[[l]])){
          class <- NULL
          prob <- NULL
          k <- 1
          # Process each item in last_token
          for (m in 1:nrow(last_token_f_var2[[l]][[g]])) {
            if (trimws(tolower(last_token_f_var2[[l]][[g]]$top5_tokens[m])) %in% valid_tokens) {
              # Filter and extract required values
              if (g == 1 | g == 2){
                if (trimws(tolower(gsub("[\n]", "", last_token_f_var2[[l]][[g]]$top5_tokens[m]))) == "in"){
                  class[k] <- "positive"
                } else if (trimws(tolower(gsub("[\n]", "", last_token_f_var2[[l]][[g]]$top5_tokens[m]))) == "de"){
                  class[k] <- "negative"
                }
              } else if (g == 3 | g == 4){
                if (trimws(tolower(gsub("[\n]", "", last_token_f_var2[[l]][[g]]$top5_tokens[m]))) == "in"){
                  class[k] <- "negative"
                } else if (trimws(tolower(gsub("[\n]", "", last_token_f_var2[[l]][[g]]$top5_tokens[m]))) == "de"){
                  class[k] <- "positive"
                }
              }

              prob[k] <- last_token_f_var2[[l]][[g]]$probability[m]
              k <- k + 1
            }
          }
          # Filter positive probabilities
          positive_probs <- prob > 0
          probs[[g]] <- data.frame(Class = class[positive_probs], Probability = prob[positive_probs])
        }
        all_prob_var2[[l]] <- probs
      }


      ## Certainty of sign per variable per causal relationship
      #var1
      var1_pos_prob_f <- NULL
      var1_neg_prob_f <- NULL
      for (w in row_index_var1) {
        var1_pos_prob_t <- 0
        var1_neg_prob_t <- 0
        for (z in 1:4) {
          for (x in 1:nrow(all_prob_var1[[w]][[z]])){
            if (all_prob_var1[[w]][[z]]$Class[[x]] == "positive") {
              var1_pos_prob_t <- var1_pos_prob_t + all_prob_var1[[w]][[z]]$Probability[[x]]
            } else if (all_prob_var1[[w]][[z]]$Class[[x]] == "negative") {
              var1_neg_prob_t <- var1_neg_prob_t + all_prob_var1[[w]][[z]]$Probability[[x]]
            }
          }
        }
        var1_pos_prob_f[w] <- ifelse((var1_pos_prob_t / 4) > 100, round((var1_pos_prob_t / 4)), round((var1_pos_prob_t / 4), 2))
        var1_neg_prob_f[w] <- ifelse((var1_neg_prob_t / 4) > 100, round((var1_neg_prob_t / 4)), round((var1_neg_prob_t / 4), 2))
      }

      #var2
      var2_pos_prob_f <- NULL
      var2_neg_prob_f <- NULL
      for (w in row_index_var2) {
        var2_pos_prob_t <- 0
        var2_neg_prob_t <- 0
        for (z in 1:4) {
          for (x in 1:nrow(all_prob_var2[[w]][[z]])){
            if (all_prob_var2[[w]][[z]]$Class[[x]] == "positive") {
              var2_pos_prob_t <- var2_pos_prob_t + all_prob_var2[[w]][[z]]$Probability[[x]]
            } else if (all_prob_var2[[w]][[z]]$Class[[x]] == "negative") {
              var2_neg_prob_t <- var2_neg_prob_t + all_prob_var2[[w]][[z]]$Probability[[x]]
            }
          }
        }
        var2_pos_prob_f[w] <- ifelse((var2_pos_prob_t / 4) > 100, round((var2_pos_prob_t / 4)), round((var2_pos_prob_t / 4), 2))
        var2_neg_prob_f[w] <- ifelse((var2_neg_prob_t / 4) > 100, round((var2_neg_prob_t / 4)), round((var2_neg_prob_t / 4), 2))
      }


      ## Add the sign to the direction df
      sign_df <- prob_df

      # var1
      #replace absent relations with 0
      var1_pos_prob_f[is.na(var1_pos_prob_f)] <- 0
      var1_neg_prob_f[is.na(var1_neg_prob_f)] <- 0

      #make sure dataframes are of equal length and add sign to sign_df
      if(length(var1_pos_prob_f) < nrow(sign_df)) {
        var1_pos_prob_f[(length(var1_pos_prob_f) + 1) : nrow(sign_df)] <- 0
      }
      if(length(var1_neg_prob_f) < nrow(sign_df)) {
        var1_neg_prob_f[(length(var1_neg_prob_f) + 1) : nrow(sign_df)] <- 0
      }

      sign_df$prob_var1_pos <- var1_pos_prob_f
      sign_df$prob_var1_neg <- var1_neg_prob_f


      # var2
      #replace absent relations with 0
      var2_pos_prob_f[is.na(var2_pos_prob_f)] <- 0
      var2_neg_prob_f[is.na(var2_neg_prob_f)] <- 0

      #make sure dataframes are of equal length and add sign to sign_df
      if(length(var2_pos_prob_f) < nrow(sign_df)) {
        var2_pos_prob_f[(length(var2_pos_prob_f) + 1) : nrow(sign_df)] <- 0
      }
      if(length(var2_neg_prob_f) < nrow(sign_df)) {
        var2_neg_prob_f[(length(var2_neg_prob_f) + 1) : nrow(sign_df)] <- 0
      }

      sign_df$prob_var2_pos <- var2_pos_prob_f
      sign_df$prob_var2_neg <- var2_neg_prob_f


      # rearrange columns
      sign_df <- sign_df[, c("var1",
                             "var2",
                             "prob_causal",
                             "prob_var1_cause",
                             "prob_var1_pos",
                             "prob_var1_neg",
                             "prob_var2_cause",
                             "prob_var2_pos",
                             "prob_var2_neg")]

    }, error = function(e) {
      cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
          "Only part of the output is returned.", sep = "\n")
    })

    raw_LLM <- list(var1 = raw_LLM_var1, var2 = raw_LLM_var2)
    logprobs_LLM <- list(var1 = logprobs_LLM_var1, var2 = logprobs_LLM_var2)

  }

  # Initialize the output list
  output <- list()

  # Add raw_LLM to output
  tryCatch({
    if (which_df == "relation") {

      # Initialize empty dataframe
      flattened_df_raw_LLM <- data.frame(relationship = integer(),
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

      # Flatten raw_LLM
      for (i in seq_along(raw_LLM)) {
        for (j in seq_along(raw_LLM[[i]])) {
          temp <- raw_LLM[[i]][[j]]
          flattened_df_raw_LLM <- rbind(flattened_df_raw_LLM,
                                        data.frame(relationship = i,
                                                   iteration = j,
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
      }

      raw_LLM_df <- flattened_df_raw_LLM

    } else if (which_df == "direction") {
      # Initialize two empty dataframes for var1 and var2
      flattened_df_var1 <- data.frame(var = character(),
                                      relationship = integer(),
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

      flattened_df_var2 <- data.frame(var = character(),
                                      relationship = integer(),
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

      # Flatten var1
      for (i in seq_along(raw_LLM_var1)) {
        for (j in seq_along(raw_LLM_var1[[i]])) {
          temp <- raw_LLM_var1[[i]][[j]]
          flattened_df_var1 <- rbind(flattened_df_var1,
                                     data.frame(var = "var1",
                                                relationship = i,
                                                iteration = j,
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
      }

      # Flatten var2
      for (i in seq_along(raw_LLM_var2)) {
        for (j in seq_along(raw_LLM_var2[[i]])) {
          temp <- raw_LLM_var2[[i]][[j]]
          flattened_df_var2 <- rbind(flattened_df_var2,
                                     data.frame(var = "var2",
                                                relationship = i,
                                                iteration = j,
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
      }

      # Combine the two dataframes
      raw_LLM_df <- rbind(flattened_df_var1, flattened_df_var2)
    }
    output$raw_LLM <- raw_LLM_df

  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })


  # Adding prob_relation_df to output
  output$sign_df <- sign_df

  if (which_df == "relation") {
    print(paste0("Total of LLM prompts: ", length(row_index) * 8))

    # give openai error if there is no output at all
    if (length(output) == 0) {
      for (i in row_index) {
        if (!is.null(raw_LLM[[i]][[1]]$error$message)) {
          stop(raw_LLM[[i]][[1]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[2]]$error$message)) {
          stop(raw_LLM[[i]][[2]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[3]]$error$message)) {
          stop(raw_LLM[[i]][[3]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[4]]$error$message)) {
          stop(raw_LLM[[i]][[4]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[5]]$error$message)) {
          stop(raw_LLM[[i]][[5]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[6]]$error$message)) {
          stop(raw_LLM[[i]][[6]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[7]]$error$message)) {
          stop(raw_LLM[[i]][[7]]$error$message)
        } else if (!is.null(raw_LLM[[i]][[8]]$error$message)) {
          stop(raw_LLM[[i]][[8]]$error$message)
        }
      }
    }

  } else if (which_df == "direction") {
    print(paste0("Total of LLM prompts: ", (length(row_index_var1) * 4) + (length(row_index_var2) * 4)))

    # give openai error if there is no output at all
    if (length(output) == 0) {
      for (i in row_index_var1) {
        if (!is.null(raw_LLM_var1[[i]][[1]]$error$message)) {
          stop(raw_LLM_var1[[i]][[1]]$error$message)
        } else if (!is.null(raw_LLM_var1[[i]][[2]]$error$message)) {
          stop(raw_LLM_var1[[i]][[2]]$error$message)
        } else if (!is.null(raw_LLM_var1[[i]][[3]]$error$message)) {
          stop(raw_LLM_var1[[i]][[3]]$error$message)
        } else if (!is.null(raw_LLM_var1[[i]][[4]]$error$message)) {
          stop(raw_LLM_var1[[i]][[4]]$error$message)
        }
      }

      for (i in row_index_var2) {
        if (!is.null(raw_LLM_var2[[i]][[1]]$error$message)) {
          stop(raw_LLM_var2[[i]][[1]]$error$message)
        } else if (!is.null(raw_LLM_var2[[i]][[2]]$error$message)) {
          stop(raw_LLM_var2[[i]][[2]]$error$message)
        } else if (!is.null(raw_LLM_var2[[i]][[3]]$error$message)) {
          stop(raw_LLM_var2[[i]][[3]]$error$message)
        } else if (!is.null(raw_LLM_var2[[i]][[4]]$error$message)) {
          stop(raw_LLM_var2[[i]][[4]]$error$message)
        }
      }
    }
  }

  return(output)
}
