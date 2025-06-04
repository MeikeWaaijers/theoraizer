library(testthat)
library(vcr)
library(theoraizer)

test_that("causal_sign() works for a both input df", {
  skip_on_cran()
  vcr::use_cassette("causal_sign", {
    rel_sign <- theoraizer::causal_sign(context = "addiction",
                                        prob_df = theoraizer::rel$relation_df[1,])
    dir_sign <- theoraizer::causal_sign(context = "addiction",
                                        prob_df = theoraizer::dir$direction_df[1,])

  })
  testthat::expect_true(!is.null(rel_sign))
  testthat::expect_true(is.list(rel_sign))
  testthat::expect_true(sum(c("raw_LLM",
                              "sign_df") %in% names(rel_sign)) == 2)
  testthat::expect_true(is.data.frame(rel_sign$raw_LLM))
  testthat::expect_true(sum(c("relationship",
                              "iteration",
                              "LLM_model",
                              "prompt",
                              "system_prompt",
                              "content",
                              "finish_reason",
                              "prompt_tokens",
                              "answer_tokens",
                              "total_tokens",
                              "error") %in% names(rel_sign$raw_LLM)) == 11)
  testthat::expect_true(is.data.frame(rel_sign$sign_df))
  testthat::expect_true(sum(c("var1",
                              "var2",
                              "prob_causal",
                              "prob_pos",
                              "prob_neg") %in% names(rel_sign$sign_df)) == 5)


  testthat::expect_true(!is.null(dir_sign))
  testthat::expect_true(is.list(dir_sign))
  testthat::expect_true(sum(c("raw_LLM",
                              "sign_df") %in% names(dir_sign)) == 2)
  testthat::expect_true(is.data.frame(dir_sign$raw_LLM))
  testthat::expect_true(sum(c("var",
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
                              "error") %in% names(dir_sign$raw_LLM)) == 12)
  testthat::expect_true(is.data.frame(dir_sign$sign_df))
  testthat::expect_true(sum(c("var1",
                              "var2",
                              "prob_causal",
                              "prob_var1_cause",
                              "prob_var1_pos",
                              "prob_var1_neg",
                              "prob_var2_cause",
                              "prob_var2_pos",
                              "prob_var2_neg") %in% names(dir_sign$sign_df)) == 9)
})

test_that("Invalid context parameter causes error", {
  prob_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_sign(context = 123,
                                                 prob_df = prob_df,
                                                 causal_threshold = 50,
                                                 LLM_model = "gpt-4",
                                                 max_tokens = 1000),
                         "'context' should be a character string or NULL.")
})

test_that("Invalid prob_df parameter causes error", {
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = "not_a_dataframe",
                                                 causal_threshold = 50,
                                                 LLM_model = "gpt-4",
                                                 max_tokens = 1000),
                         "'prob_df' should be a dataframe.")
})

test_that("Invalid columns in prob_df (relation) cause error", {
  prob_df <- data.frame(a = c("A", "B"), b = c("B", "C"), c = c(60, 70))
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = prob_df,
                                                 causal_threshold = 50,
                                                 LLM_model = "gpt-4",
                                                 max_tokens = 1000),
                         "'prob_df' should have three columns named 'var1', 'var2' and 'prob_causal'.")
})

test_that("Invalid columns in prob_df (direction) cause error", {
  prob_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70), a = c(80, 90), b = c(10, 20))
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = prob_df,
                                                 causal_threshold = 50,
                                                 LLM_model = "gpt-4",
                                                 max_tokens = 1000),
                         "'prob_df' should have five columns named 'var1', 'var2', 'prob_causal', 'prob_var1_cause', 'prob_var2_cause'.")
})

test_that("Invalid causal_threshold parameter causes error", {
  prob_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = prob_df,
                                                 causal_threshold = -10,
                                                 LLM_model = "gpt-4",
                                                 max_tokens = 1000),
                         "'causal_threshold' should be a number between 0 and 100, and cannot have more than two decimal points.")
})

test_that("Invalid LLM_model parameter causes error", {
  prob_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = prob_df,
                                                 causal_threshold = 50,
                                                 LLM_model = "invalid_model",
                                                 max_tokens = 1000),
                         "'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', 'mixtral', or 'llama-3'.")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  prob_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = prob_df,
                                                 causal_threshold = 50,
                                                 LLM_model = "gpt-4o",
                                                 max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  prob_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(theoraizer::causal_sign(context = "Climate Change",
                                                 prob_df = prob_df,
                                                 causal_threshold = 50,
                                                 LLM_model = "gpt-3.5-turbo",
                                                 max_tokens = 4000),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})
