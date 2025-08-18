library(testthat)
library(vcr)
library(theoraizer)

test_that("causal_direction() works", {
  skip_on_cran()
  vcr::use_cassette("causal_direction", {
    dir <- theoraizer::causal_direction(topic = "addiction",
                                        relation_df = theoraizer::rel$relation_df[1,])

  })
  testthat::expect_true(!is.null(dir))
  testthat::expect_true(is.list(dir))
  testthat::expect_true(all(c("raw_LLM",
                              "direction_df") %in% names(dir)))
  testthat::expect_true(is.data.frame(dir$raw_LLM))
  testthat::expect_true(all(c("var",
                              "relationship",
                              "iteration",
                              "LLM_model",
                              "prompt",
                              "system_prompt",
                              "content",
                              "finish_reason",
                              "prompt_tokens",
                              "answer_tokens",
                              "total_tokens",
                              "error") %in% names(dir$raw_LLM)))
  testthat::expect_true(is.data.frame(dir$direction_df))
  testthat::expect_true(all(c("var1",
                              "var2",
                              "prob_causal",
                              "prob_var1_cause",
                              "prob_var2_cause") %in% names(dir$direction_df)))
})

test_that("Invalid topic parameter causes error", {
  relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_direction(topic = 123,
                                                      relation_df = relation_df,
                                                      causal_threshold = 50,
                                                      LLM_model = "gpt-4",
                                                      max_tokens = 1000),
                         "'topic' should be a character string or NULL.")
})

test_that("Invalid relation_df parameter causes error", {
  testthat::expect_error(theoraizer::causal_direction(topic = "Climate Change",
                                                      relation_df = "not_a_dataframe",
                                                      causal_threshold = 50,
                                                      LLM_model = "gpt-4",
                                                      max_tokens = 1000),
                         "'relation_df' should be a dataframe.")
})

test_that("Invalid columns in relation_df cause error", {
  relation_df <- data.frame(a = c("A", "B"), b = c("B", "C"), c = c(60, 70))
  testthat::expect_error(theoraizer::causal_direction(topic = "Climate Change",
                                                      relation_df = relation_df,
                                                      causal_threshold = 50,
                                                      LLM_model = "gpt-4",
                                                      max_tokens = 1000),
                         "'relation_df' should have three columns named 'var1', 'var2' and 'prob_causal'.")
})

test_that("Invalid causal_threshold parameter causes error", {
  relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_direction(topic = "Climate Change",
                                                      relation_df = relation_df,
                                                      causal_threshold = -10,
                                                      LLM_model = "gpt-4",
                                                      max_tokens = 1000),
                         "'causal_threshold' should be a number between 0 and 100, and cannot have more than two decimal points.")
})

test_that("Invalid LLM_model parameter causes error", {
  relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_direction(topic = "Climate Change",
                                                      relation_df = relation_df,
                                                      causal_threshold = 50,
                                                      LLM_model = "invalid_model",
                                                      max_tokens = 1000),
                         "'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', 'mixtral', or 'llama-3'.")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_direction(topic = "Climate Change",
                                                      relation_df = relation_df,
                                                      causal_threshold = 50,
                                                      LLM_model = "gpt-4o",
                                                      max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_direction(topic = "Climate Change",
                                                      relation_df = relation_df,
                                                      causal_threshold = 50,
                                                      LLM_model = "gpt-3.5-turbo",
                                                      max_tokens = "4000"),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})
