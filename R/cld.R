# Program Name: theoraizer
# Description: The cld function can be used to quickly generate a Causal Loop Diagram (CLD).
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
### CLD function

## Function manual

#' Quickly create a CLD using a LLM
#'
#' @description
#' The \code{cld()} function can be used to quickly generate a Causal Loop Diagram (CLD). This function uses the \code{\link{causal_relation}}, \code{\link{causal_direction}}, \code{\link{causal_sign}}, and \code{\link{cld_plot}} functions included in this R package. For greater control and access to more outputs, we recommend using these functions individually.
#'
#' @usage
#' cld(topic,
#'     variable_list,
#'     plot = TRUE,
#'     LLM_model = "gpt-4o",
#'     max_tokens = 2000,
#'     update_key = FALSE)
#' @details
#' To create a fully fledged theory from scratch, the functions in this R-packaged should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}}
#'
#' @param topic A character vector specifying the topic for which a theory should be developed. If it is not feasible to identify a particular topic, the parameter can be set to NULL.
#' @param variable_list A vector containing all variables that need to be included in the theory.
#' @param plot If \code{plot = TRUE} (default), the function will generate network plot(s) visualizing the edge list(s).
#' @inheritParams var_list
#'
#' @returns
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'     \itemize{
#'       \item \code{which_fun}: Which function the raw output comes from.
#'       \item \code{var}: Which variable is put in the prompt as cause variable. (NA for causal_relation function)
#'       \item \code{relationship}: Which variable pair.
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
#'     }
#' } \cr
#' \itemize{
#'   \item \code{sign_df}: A dataframe with nine columns:
#'     \itemize{
#'       \item \code{var1}: Variable 1 of a unique variable pair.
#'       \item \code{var2}: Variable 2 of a unique variable pair.
#'       \item \code{prob_causal}: Probability of the presence of a causal relationship between var1 and var2.
#'       \item \code{prob_var1_cause}: Probability of var1 being a cause variable.
#'       \item \code{prob_var1_pos}: Probability of a positive relationship where var1 is the cause.
#'       \item \code{prob_var1_neg}: Probability of a negative relationship where var1 is the cause.
#'       \item \code{prob_var2_cause}: Probability of var2 being a cause variable.
#'       \item \code{prob_var2_pos}: Probability of a positive relationship where var1 is the cause.
#'       \item \code{prob_var2_neg}: Probability of a negative relationship where var1 is the cause.
#'     }
#' } \cr
#' \itemize{
#'   \item
#' A plot and an edge list.
#' An edge list is a list containing all the edges between nodes. These nodes symbolise variables, while the edges denote the causal relationships that link these variables.
#'
#' The specific outputted edge list is \code{dir_signs_edge_list}.
#' This is an edge list for a direction probability dataframe with sign indication (for more information about different edge lists see \code{\link{cld_plot}}).
#' \cr
#' \cr
#' \code{edge_list}: An edge list with 4 columns:
#' \itemize{
#'       \item \code{from}: Cause variable.
#'       \item \code{to}: Dependent variable.
#'       \item \code{weight}: Probability of a causal relationship between the Cause and Dependent variable.
#'       \item \code{sign}: Sign of the causal relationship (either Positive, Negative or Uncertain).
#'     }
#' }
#'
#' @references \url{https://platform.openai.com}
#'
#' @author Meike Waaijers
#'
#' @note The function and its output should be approached with caution. Depending on the specific LLM used, there may be financial implications. Furthermore, we wish to emphasise that the answers generated by an LLM should not be taken as absolute truth.
#'
#' @seealso
#' \code{\link{var_list}},
#' \code{\link{causal_relation}},,
#' \code{\link{causal_direction}},
#' \code{\link{causal_sign}},
#' \code{\link{cld_plot}},
#'
#' @examples
#' \dontrun{
#' ## Example input (topic = "addiction")
#' data("vars")
#' vars$final_list
#'
#' #---------------------------------------------------------------------------
#' ## Default
#' # For a readily available, pre-made output example see: data("cld_example")
#'
#' cld_example <- cld(topic = "addiction",
#'                    variable_list = vars$final_list)
#'
#' # Check output
#' cld_example$sign_df
#' cld_example$edge_list
#' }
#' @import httr
#' @import qgraph
#' @import keyring
#' @export

cld <- function(topic,
                variable_list,
                plot = TRUE,
                LLM_model = "gpt-4o",
                max_tokens = 2000,
                update_key = FALSE) {

  #validate input
  stopifnot("'topic' should be a character string or NULL." = is.character(topic) | is.null(topic))
  stopifnot("'variable_list' should be a vector containing more than one variables." = is.vector(variable_list) && length(variable_list) > 1)
  stopifnot("All entries in 'variable_list' should be character strings." =
              all(sapply(variable_list, is.character)))
  stopifnot("'plot' should be a logical value." = is.logical(plot))
  stopifnot("'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', or 'mixtral'." =
              LLM_model %in% c("mixtral", "gpt-4o", "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo"))
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

  causrel <- NULL
  causdir <- NULL
  caussign <- NULL
  theoryplot <- NULL

  tryCatch({
    causrel <- causal_relation(topic = topic,
                               variable_list = variable_list,
                               LLM_model = LLM_model,
                               max_tokens = max_tokens)
  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output from relation function -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  tryCatch({
    causdir <- causal_direction(topic = topic,
                                relation_df = causrel$relation_df,
                                LLM_model = LLM_model,
                                max_tokens = max_tokens)
  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output from direction function -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  tryCatch({
    caussign <- causal_sign(topic = topic,
                            prob_df = causdir$direction_df,
                            LLM_model = LLM_model,
                            max_tokens = max_tokens)

  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output from sign function -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  if (plot == TRUE) {
    tryCatch({
      theoryplot <- cld_plot(topic = topic,
                             dir_sign_df = caussign$sign_df)

    }, error = function(e) {
      cat(paste0("Warning: Unable to create plot -> ", e$message, "."),
          "Only part of the output is returned.", sep = "\n")
    })
  }

  # Initialize the output list
  output <- list()
  raw_LLM <- NULL
  tryCatch({
    # Add raw dataframes to output
    raw_LLM <- rbind(cbind(which_fun = "causal_relation", var = NA, causrel$raw_LLM),
                     cbind(which_fun = "causal_direction", causdir$raw_LLM),
                     cbind(which_fun = "causal_sign", caussign$raw_LLM))

  }, error = function(e) {
    cat(paste0("Warning: Unable to include raw output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  output$raw_LLM <- raw_LLM
  output$sign_df <- caussign$sign_df

  if(plot == TRUE){
    output$edge_list <- theoryplot$dir_signs_edge_list
  }



  return(output)
}
