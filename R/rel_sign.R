# Program Name: theoraizer
# Description: This data set contains the probabilities of the presence and the sign of causal relationships between various variables related to the topic of addiction. The data was generated using the "gpt-4o" Large Language Model (LLM).
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
### rel_sign data set


#' rel_sign data
#'
#' This data set contains the probabilities of the presence and the sign of
#' causal relationships between various variables related to the topic of addiction.
#' The data was generated using the \code{\link{causal_sign}} function and the "gpt-4o" Large Language Model (LLM).
#'
#' @format A list containing the following components:
#' \describe{
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'     \describe{
#'     \itemize{
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
#'     }} \cr
#'   \item \code{sign_df}: A dataframe with five columns representing the probability of the presence and the sign of causal relationships between various variable.
#'     \describe{
#'     \itemize{
#'       \item \code{var1}: Variable 1 of a unique variable pair.
#'       \item \code{var2}: Variable 2 of a unique variable pair.
#'       \item \code{prob_causal}: Probability of the presence of a causal relationship between var1 and var2.
#'       \item \code{prob_pos}: Probability of a positive relationship.
#'       \item \code{prob_neg}: Probability of a negative relationship.
#'     }}
#' }
#' }
#'
#' @source Generated using the OpenAI platform: \url{https://platform.openai.com}
#'
#' @examples
#' data("rel_sign")
"rel_sign"
