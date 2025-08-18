# Program Name: theoraizer
# Description: This data set contains a list of 4 edge lists related to the topic of addiction. The data was generated using the "gpt-4o" Large Language Model (LLM).
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
### edge_lists data set

## Data description

#' edge_lists data
#'
#' This data set contains a list of 4 edge lists related to the topic of addiction.
#' The data was generated using the \code{\link{cld_plot}} function and the "gpt-4o" Large Language Model (LLM).
#'
#' @format A list containing the following components:
#' \describe{
#' \itemize{
#'   \item \code{rel_edge_list}: Edge list for a relation probability dataframe (\code{relation_df}) input.
#'     \describe{
#'     \itemize{
#'       \item \code{from}: The "from" node, otherwise known as the "cause" variable.
#'       \item \code{to}: The "to" node, otherwise known as the "dependent" variable.
#'       \item \code{weight}: The weights associated with the causal relationship. In this case, the weights refer to the probability of there being a causal relationship.
#'     }} \cr
#'   \item \code{dir_edge_list}: Edge list for a direction probability dataframe (\code{direction_df}) input.
#'     \describe{
#'     \itemize{
#'       \item \code{from}: The "from" node, otherwise known as the "cause" variable.
#'       \item \code{to}: The "to" node, otherwise known as the "dependent" variable.
#'       \item \code{weight}: The weights associated with the causal relationship. In this case, the weights refer to the probability of there being a causal relationship.
#'     }} \cr
#'   \item \code{rel_sign_edge_list}: Edge list for a relation probability dataframe with sign indication (relation \code{sign_df}) input.
#'     \describe{
#'     \itemize{
#'       \item \code{from}: The "from" node, otherwise known as the "cause" variable.
#'       \item \code{to}: The "to" node, otherwise known as the "dependent" variable.
#'       \item \code{weight}: The weights associated with the causal relationship. In this case, the weights refer to the probability of there being a causal relationship.
#'       \item \code{sign}: The sign of the causal relationship (can be either "positive", "negative", or "uncertain").
#'     }} \cr
#'   \item \code{dir_sign_edge_list}: Edge list for a direction probability dataframe with sign indication (direction \code{sign_df}) input.
#'     \describe{
#'     \itemize{
#'       \item \code{from}: The "from" node, otherwise known as the "cause" variable.
#'       \item \code{to}: The "to" node, otherwise known as the "dependent" variable.
#'       \item \code{weight}: The weights associated with the causal relationship. In this case, the weights refer to the probability of there being a causal relationship.
#'       \item \code{sign}: The sign of the causal relationship (can be either "positive", "negative", or "uncertain").
#'     }}
#' }
#' }
#'
#' @source Generated using the OpenAI platform: \url{https://platform.openai.com}
#'
#' @examples
#' data("edge_lists")
"edge_lists"
