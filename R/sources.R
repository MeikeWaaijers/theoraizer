# Program Name: theoraizer
# Description: This data set contains an edge list of causal relationships related to the topic of addiction, together with scientific sources and short explanatory summaries of how each source discusses the corresponding relationship. The data was generated using the "gpt-4.1" Large Language Model (LLM).
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
### sources data set


#' sources data
#'
#' This data set contains an edge list of causal relationships related to the topic
#' of addiction, enriched with scientific sources and explanatory summaries of how
#' each source discusses the corresponding relationship.
#' The data was generated using the \code{\link{find_source}} function and the "gpt-4.1" Large Language Model (LLM).
#'
#' @format A list containing the following components:
#' \describe{
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along
#'   with metadata, including:
#'     \describe{
#'     \itemize{
#'       \item \code{relationship}: Which variable pair.
#'       \item \code{LLM_model}: LLM model used.
#'       \item \code{prompt}: Prompt used.
#'       \item \code{content}: Unprocessed LLM output.
#'       \item \code{finish_reason}: Reason the LLM stopped generating output.
#'       \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
#'       \item \code{answer_tokens}: Number of tokens used for the LLM answer.
#'       \item \code{total_tokens}: Total number of tokens used.
#'       \item \code{error}: Error message, if any occurred.
#'     }} \cr
#'   \item \code{edge_list_with_sources}: A dataframe containing the original edge
#'   list columns, enriched with sources and summaries:
#'     \describe{
#'     \itemize{
#'       \item \code{from}: The "from" node, otherwise known as the "cause" variable.
#'       \item \code{to}: The "to" node, otherwise known as the "dependent" variable.
#'       \item \code{weight}: The weights associated with the causal relationship. In this case, the weights refer to the probability of there being a causal relationship.
#'       \item \code{sign}: (Optional) The sign of the relationship (can be either "Positive", "Negative", or "Uncertain").
#'       \item \code{explanation}: A short explanatory summary of how the identified source discusses the causal relationship.
#'       \item \code{sources}: One or more citation links returned by the LLM, stored as a semicolon-separated string.
#'     }}
#' }
#' }
#'
#' @source Generated using the OpenAI platform: \url{https://platform.openai.com}
#'
#' @examples
#' data("sources")
"sources"
