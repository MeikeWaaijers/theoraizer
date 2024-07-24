# Program Name: theoraizer
# Description: This function extracts the five highest log probabilities for every Large Language Model (LLM) token in the LLM output.
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

## logprob helper function

LLM_logprobs <- function(raw_content,
                         LLM_model = LLM_model) {

  length <- length(raw_content$choices[[1]]$logprobs$content)
  logprobs_dfs <- vector("list", length = length)

  for (j in seq_along(logprobs_dfs)) {
    logprobs_list <- raw_content$choices[[1]]$logprobs$content[[j]]$top_logprobs

    top5_tokens <- vector("list", length = length(logprobs_list))
    top5_logprobs <- numeric(length(logprobs_list))
    top5_probabilities <- numeric(length(logprobs_list))

    for (i in seq_along(logprobs_list)) {
      top5_tokens[[i]] <- logprobs_list[[i]]$token
      top5_logprobs[i] <- logprobs_list[[i]]$logprob

      top5_probabilities[i] <- round(exp(top5_logprobs[i]) * 100, 2)
    }

    logprobs_dfs[[j]] <- data.frame(top5_tokens = unlist(top5_tokens),
                                    logprob = top5_logprobs,
                                    probability = top5_probabilities)
  }

  return(logprobs_dfs)
}
