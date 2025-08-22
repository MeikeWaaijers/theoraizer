library(testthat)
library(vcr)
library(theoraizer)

test_that("find_source() works", {
  skip_on_cran()
  vcr::use_cassette("find_source", {
    source <- theoraizer::find_source(topic = "addiction",
                                      edge_list = theoraizer::edge_lists$dir_sign_edge_list[1:5, 1:4]) # keep small for speed)
  })
  expect_true(!is.null(source))
  expect_true(is.list(source))
  expect_true(all(c("raw_LLM",
                    "edge_list_with_sources") %in% names(source)))

  # raw_LLM checks
  expect_true(is.data.frame(source$raw_LLM))
  expect_true(all(c("relationship",
                    "LLM_model",
                    "prompt",
                    "content",
                    "finish_reason",
                    "prompt_tokens",
                    "answer_tokens",
                    "total_tokens",
                    "error") %in% names(source$raw_LLM)))

  # edge_list_with_sources checks
  expect_true(is.data.frame(source$edge_list_with_sources))
  expect_true(all(c("from",
                    "to",
                    "weight",
                    "explanation",
                    "sources") %in% names(source$edge_list_with_sources)))
})

test_that("Invalid topic parameter causes error", {
  edge_list <- data.frame(from = c("A", "B"),
                          to = c("B", "C"),
                          weight = c(60, 70))
  expect_error(theoraizer::find_source(topic = 123,
                                       edge_list = edge_list,
                                       causal_threshold = 50,
                                       LLM_model = "gpt-4.1",
                                       max_tokens = 2000),
               "'topic' should be a single non-empty character string or NULL.")
})

test_that("Invalid edge_list parameter causes error", {
  expect_error(theoraizer::find_source(topic = "addiction",
                                       edge_list = "not_a_dataframe",
                                       LLM_model = "gpt-4.1",
                                       max_tokens = 2000),
               "'edge_list' should be a data frame.")
})

test_that("Invalid columns in edge_list cause error", {
  edge_list <- data.frame(a = c("A", "B"),
                          b = c("B", "C"),
                          c = c(60, 70))
  expect_error(theoraizer::find_source(topic = "addiction",
                                       edge_list = edge_list,
                                       LLM_model = "gpt-4.1",
                                       max_tokens = 2000),
               "'edge_list' should have at least three columns named 'from', 'to', and 'weight'.")
})

test_that("Invalid sign values cause error", {
  edge_list <- data.frame(from = c("A"),
                          to = c("B"),
                          weight = c(80),
                          sign = c("BadSign"))
  expect_error(theoraizer::find_source(topic = "addiction",
                                       edge_list = edge_list,
                                       LLM_model = "gpt-4.1",
                                       max_tokens = 2000),
               "All entries in 'edge_list\\$sign' should be either 'Positive', 'Negative', or 'Uncertain'.")
})

test_that("Invalid LLM_model parameter causes error", {
  edge_list <- data.frame(from = c("A", "B"),
                          to = c("B", "C"),
                          weight = c(60, 70))
  expect_error(theoraizer::find_source(topic = "addiction",
                                       edge_list = edge_list,
                                       LLM_model = "invalid_model",
                                       max_tokens = 2000),
               "'LLM_model' should be 'gpt-4.1'")
})

test_that("Invalid max_tokens parameter causes error for gpt-4.1", {
  edge_list <- data.frame(from = c("A", "B"),
                          to = c("B", "C"),
                          weight = c(60, 70))
  expect_error(theoraizer::find_source(topic = "addiction",
                                       edge_list = edge_list,
                                       LLM_model = "gpt-4.1",
                                       max_tokens = 7000),
               "For 'gpt-4.1', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})
