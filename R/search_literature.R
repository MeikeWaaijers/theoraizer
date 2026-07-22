# Program Name: CLDassist
# Description: In the search_literature function a Large Language Model (LLM) is asked to identify relevant literature for each variable pair in a Causal Loop Diagram (CLD).
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


#### CLDassist
### Search literature function

## Function manual

#' Search Relevant Literature for Causal Relationships
#'
#' @description
#' \code{search_literature()} iterates over an edge list of putative causal relationships and asks a Large Language Model (LLM) to search and summarize relevant literature for each relationship. For every variable pair, the LLM is asked to search for literature discussing the relationship and to report what the literature indicates, without being directed toward finding evidence for or against a specific conclusion. This applies regardless of any weight or sign associated with the pair in the input edge list.
#'
#' @usage
#' search_literature(topic,
#'                   edge_list = NULL,
#'                   scientific = TRUE,
#'                   LLM_model = "gpt-4.1",
#'                   max_tokens = 2000,
#'                   update_key = FALSE,
#'                   custom_llm_fn = NULL)
#'
#' @details
#' To create a CLD from scratch, the functions in this R-package should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}} --> \code{\link{search_literature}}
#'
#' @param topic A character vector specifying the topic for which a CLD should be developed. If it is not feasible to identify a particular topic, the argument can be set to NULL.
#' @param edge_list A data frame representing a causal edge list, with one row per putative causal relationship. Must contain at least two columns: \code{from}, a character column naming the cause variable; and \code{to}, a character column naming the dependent variable. The edge list may contain additional columns (e.g., \code{weight}, \code{sign}) produced by other \code{CLDassist} functions; these are accepted but not used by \code{search_literature()}.
#' @param scientific If \code{scientific = TRUE } (default), the LLM is asked to search scientific literature. If \code{FALSE}, any credible source is allowed.
#' @param LLM_model The LLM model that should be used to generate output. As of now, only \code{"gpt-4.1"} is available for this function. Other models may be added in future updates.
#' @param max_tokens The maximum number of tokens the LLM should generate. Be careful when adjusting this argument. Reducing the maximum token limit will reduce the cost but may result in incomplete answers. Conversely, increasing the token limit can be advantageous for obtaining more detailed responses. The maximum number of tokens depends on the model. As of now, only \code{"gpt-4.1"} is supported for this function, with a maximum token limit of \code{6000}.
#' @inheritParams var_list
#' @param custom_llm_fn Optionally, a custom function to use instead of the built-in LLM models. If provided, the \code{LLM_model}, \code{max_tokens}, and \code{update_key} arguments are ignored; handle those inside your custom function. The function must accept \code{prompt} as an argument and return a list with at minimum \code{$output} (the response text as a character string), and \code{$sources} (a character vector of citation URLs found by the model). Since the purpose of this function is to look for literature, your custom function should use an LLM with web search or browsing capabilities so that \code{$sources} can be populated. If \code{$sources} is omitted or \code{NULL}, the \code{url} column in the output will be \code{NA}.
#'
#'
#' @returns
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'     \itemize{
#'       \item \code{relationship}: Which variable pair.
#'       \item \code{LLM_model}: LLM model used.
#'       \item \code{prompt}: The prompt asked to the LLM.
#'       \item \code{content}: Unprocessed LLM output.
#'       \item \code{finish_reason}: Reason the LLM stopped generating output.
#'       \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
#'       \item \code{answer_tokens}: Number of tokens used for the LLM answer.
#'       \item \code{total_tokens}: Total number of tokens used.
#'       \item \code{error}: Error message, if any occurred.
#'     }
#' } \cr
#' \itemize{
#'   \item \code{edge_list_with_lit}: The input \code{edge_list} with all its original columns retained, plus two new ones:
#'     \itemize{
#'     \item \code{url}: One or more citation links returned by the model, stored as a semicolon separated string.
#'     \item \code{explanation}: A short summary of the literature identified by the model, typically including the key findings and how they relate to the causal relationship. May also note when no direct publication was found.
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
#' \code{\link{cld}},
#' \code{\link{var_list}},
#' \code{\link{causal_relation}},
#' \code{\link{causal_direction}},
#' \code{\link{causal_sign}},
#' \code{\link{cld_plot}}
#'
#' @examples
#' \dontrun{
#' ## Example input (topic = "addiction")
#' data("edge_lists")
#'
#' # Use the direction + sign edge list (columns: from, to, weight, sign)
#' input <- edge_lists$dir_sign_edge_list
#'
#' #---------------------------------------------------------------------------
#' ## Default
#' # For a readily available, pre-made output example see: data("literature")
#' literature <- search_literature(topic = "addiction",
#'                                 edge_list = input)
#'
#' # Check output
#' literature$edge_list_with_lit
#' }
#'
#' @import httr
#' @import utils
#' @import keyring
#' @export

## search_literature function
search_literature <- function(topic,
                              edge_list = NULL,
                              scientific = TRUE,
                              LLM_model = "gpt-4.1",
                              max_tokens = 2000,
                              update_key = FALSE,
                              custom_llm_fn = NULL) {

  # Validate input
  stopifnot(
    "'topic' should be a single non-empty character string or NULL." =
      is.null(topic) ||
      (is.character(topic) && length(topic) == 1L && !is.na(topic) && nzchar(trimws(topic)))
  )
  stopifnot("'edge_list' should be a data frame." = is.data.frame(edge_list))
  stopifnot("'edge_list' should have at least two columns named 'from' and 'to'." =
              ncol(edge_list) >= 2 && all(c("from", "to") %in% names(edge_list)))
  stopifnot("All entries in 'edge_list$from' and 'edge_list$to' should be character strings." =
              all(sapply(edge_list$from, is.character)) && all(sapply(edge_list$to, is.character)))

  stopifnot("'LLM_model' should be 'gpt-4.1'" =
              !is.null(custom_llm_fn) || LLM_model == "gpt-4.1")
  stopifnot("For 'gpt-4.1', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !is.null(custom_llm_fn) || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens > 0 && max_tokens <= 6000))

  if (!is.null(custom_llm_fn)) {
    stopifnot("'custom_llm_fn' must be a function accepting 'prompt' and 'system_prompt'." =
                is.function(custom_llm_fn) &&
                all(c("prompt", "system_prompt") %in% names(formals(custom_llm_fn))))
    if (update_key) message("'update_key' is ignored when 'custom_llm_fn' is provided.")
    message("'LLM_model' and 'max_tokens' are ignored when 'custom_llm_fn' is provided.")
    message("Warning: this function is designed to search literature for causal relationships. Your custom function should use an LLM with web search or browsing capabilities and return '$sources' (a character vector of citation URLs). Without it, the 'url' entries in the output will be NA and this function will provide little value over a standard LLM call.")}

  ## Load and prepare prompt data
  prompt_file_path <- system.file("extdata", "prompts.csv", package = "CLDassist")
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

  literature_prompts <- prompts_data[prompts_data$Function == "literature", ]

  n_edges <- nrow(edge_list)

  # Initialize storage
  runs <- vector("list", n_edges * 2)
  raw_LLM <- vector("list", n_edges * 2)
  all_literature <- vector("list", n_edges * 2)
  explanation <- vector("list", n_edges * 2)

  # Which source
  if (scientific) {
    source_type <- "scientific publications"
  } else {
    source_type <- "sources"
  }

  idx <- 0

  ## Loop over all edges, and for each edge, over both variable orders
  for (j in 1:n_edges) {

    message(sprintf("Processing edge: %d/%d", j, n_edges))

    var_1 <- edge_list[j, "from"]
    var_2 <- edge_list[j, "to"]

    # Store the two runs for this edge locally, so they can be combined right after.
    edge_explanations <- vector("list", 2)
    edge_literature <- vector("list", 2)

    for (o in 1:2) {

      idx <- idx + 1

      # Make prompt
      if (o == 1) {
        template <- if (is.null(topic) || !nzchar(trimws(topic))) literature_prompts$Prompt[1] else literature_prompts$Prompt[2]
      } else {
        template <- if (is.null(topic) || !nzchar(trimws(topic))) literature_prompts$Prompt[3] else literature_prompts$Prompt[4]
      }

      prompt <- gsub("\\((source_type)\\)", source_type,
                     gsub("\\((var_1)\\)", var_1,
                          gsub("\\((var_2)\\)", var_2,
                               template)))
      if (!is.null(topic) && nzchar(trimws(topic))) {
        prompt <- gsub("\\((topic)\\)", topic, prompt)
      }

      tryCatch({
        result <- .call_llm(prompt = prompt,
                            system_prompt = NULL,
                            LLM_model = LLM_model,
                            max_tokens = max_tokens,
                            update_key = update_key,
                            custom_llm_fn = custom_llm_fn)

        update_key <- FALSE # make sure api key is only updated once

        runs[[idx]] <- result$output
        all_literature[[idx]] <- result$sources
        raw_LLM[[idx]] <- c(prompt = prompt, result$raw_content)

        edge_explanations[[o]] <- result$output
        edge_literature[[o]] <- result$sources

      }, error = function(e) {
        message(sprintf("Error at edge %d, order %d (%s -> %s): %s", j, o, var_1, var_2, e$message))
        runs[[idx]] <- NA
        all_literature[idx] <- list(NULL)
        raw_LLM[[idx]] <- list(prompt = prompt, error = e$message)

        edge_explanations[[o]] <<- NA
        edge_literature[[o]] <<- NULL
      })
    }
  }


  ## Clean LLM output
  for (k in seq_along(runs)) {
    tryCatch({
      if (is.null(runs[[k]]) || is.na(runs[[k]])) stop("Empty or missing run")
      text <- runs[[k]]
      text <- gsub('\\\\\"', '"', text)
      text <- gsub('\\\\n', '\n', text)
      text <- gsub('\\(\\[[^\\]]+\\]\\((.*?)\\)\\)', '(\\1)', text, perl = TRUE)
      text <- gsub('\\[[^\\]]+\\]\\((.*?)\\)', '(\\1)', text, perl = TRUE)
      text <- gsub('[[:space:]]+', ' ', text)
      text <- trimws(text)
      text <- gsub('"', "'", text)
      explanation[[k]] <- text
    }, error = function(e) {
      message(sprintf("Cleaning error at index %d: %s", k, e$message))
      explanation[[k]] <- NA
    })
  }

  ## Combine results: for each edge, merge both query rounds into a single explanation string and a single literature string
  edge_list_out <- edge_list

  combined_explanation <- character(n_edges)
  combined_literature <- character(n_edges)

  for (j in seq_len(n_edges)) {

    idx_1 <- (j - 1) * 2 + 1
    idx_2 <- (j - 1) * 2 + 2

    exp_1 <- explanation[[idx_1]]
    exp_2 <- explanation[[idx_2]]
    exp_1 <- if (is.null(exp_1) || (length(exp_1) == 1 && is.na(exp_1))) "" else exp_1
    exp_2 <- if (is.null(exp_2) || (length(exp_2) == 1 && is.na(exp_2))) "" else exp_2
    combined_explanation[j] <- trimws(paste(c(exp_1, exp_2), collapse = " "))

    lit_1 <- all_literature[[idx_1]]
    lit_2 <- all_literature[[idx_2]]
    combined_lit <- c(lit_1, lit_2)
    combined_lit <- combined_lit[!is.na(combined_lit) & nzchar(combined_lit)]
    combined_lit <- unique(combined_lit)
    combined_literature[j] <- if (length(combined_lit) == 0) NA_character_ else paste(combined_lit, collapse = "; ")
  }

  edge_list_out$url <- combined_literature
  edge_list_out$explanation <- combined_explanation

  # Add raw_LLM to output
  tryCatch({
    # Initialize empty dataframe
    flattened_df_raw_LLM <- data.frame(relationship = integer(),
                                       LLM_model = character(),
                                       prompt = character(),
                                       content = character(),
                                       finish_reason = character(),
                                       prompt_tokens = numeric(),
                                       answer_tokens = numeric(),
                                       total_tokens = numeric(),
                                       error = character(),
                                       stringsAsFactors = FALSE)

    for (i in seq_along(raw_LLM)) {

      temp <- raw_LLM[[i]]
      relationship_id <- ceiling(i / 2)

      flattened_df_raw_LLM <- rbind(flattened_df_raw_LLM,
                                    data.frame(relationship = relationship_id,
                                               LLM_model = temp$LLM_model,
                                               prompt = temp$prompt,
                                               content = temp$content,
                                               finish_reason = temp$finish_reason,
                                               prompt_tokens = temp$prompt_tokens,
                                               answer_tokens = temp$answer_tokens,
                                               total_tokens = temp$total_tokens,
                                               error = ifelse(is.null(temp$error), NA, temp$error),
                                               stringsAsFactors = FALSE))
    }

  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })

  # Final check
  if (all(is.na(unlist(explanation)))) {
    for (i in seq_along(raw_LLM)) {
      err <- try(raw_LLM[[i]]$error$message, silent = TRUE)
      if (!inherits(err, "try-error") && !is.null(err)) stop(err)
    }
    stop("LLM failed to produce any usable output, and no error message was found.")
  }

  message(sprintf("Total of LLM prompts: %d", length(runs)))

  output <- list(raw_LLM = flattened_df_raw_LLM,
                 edge_list_with_lit = edge_list_out)


  return(output)
}
