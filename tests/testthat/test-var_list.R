library(testthat)
library(vcr)
library(theoraizer)

test_that("var_list() works", {
  skip_on_cran()
  vcr::use_cassette("var_list", {
    vars <- theoraizer::var_list(topic = "addiction",
                                 n_final = 2,
                                 n_variables = 2)
  })
  testthat::expect_true(!is.null(vars))
  testthat::expect_true(is.list(vars))
  testthat::expect_true(all(c("raw_LLM",
                              "all_vars",
                              "final_list") %in% names(vars)))

  # raw_LLM checks
  testthat::expect_true(is.data.frame(vars$raw_LLM))
  testthat::expect_true(all(c("function_part",
                              "iteration",
                              "LLM_model",
                              "prompt",
                              "system_prompt",
                              "content",
                              "finish_reason",
                              "prompt_tokens",
                              "answer_tokens",
                              "total_tokens",
                              "error") %in% names(vars$raw_LLM)))
  testthat::expect_equal(length(vars$final_list), 2)
})

#vars_list check
test_that("Invalid topic parameter causes error", {
  testthat::expect_error(theoraizer::var_list(topic = 123,
                                              n_final = 5,
                                              n_variables = 10,
                                              LLM_model = "gpt-4",
                                              max_tokens = 1000),
                         "'topic' should be a single non-empty character string.")
})

test_that("Invalid n_variables parameter causes error", {
  testthat::expect_error(theoraizer::var_list(topic = "Climate Change",
                                              n_final = 5,
                                              n_variables = -1,
                                              LLM_model = "gpt-4",
                                              max_tokens = 1000),
                         "'n_variables' should be a whole number above 0 or the input should be 'all'.")
})

test_that("Invalid n_final parameter causes error", {
  testthat::expect_error(theoraizer::var_list(topic = "Climate Change",
                                              n_final = -5,
                                              n_variables = 10,
                                              LLM_model = "gpt-4",
                                              max_tokens = 1000),
                         "'n_final' should be a whole number above 0.")
})

test_that("Invalid LLM_model parameter causes error", {
  testthat::expect_error(theoraizer::var_list(topic = "Climate Change",
                                              n_final = 5,
                                              n_variables = 10,
                                              LLM_model = "invalid_model",
                                              max_tokens = 1000),
                         "'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', 'mixtral', or 'llama-3'.")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  testthat::expect_error(theoraizer::var_list(topic = "Climate Change",
                                              n_final = 5,
                                              n_variables = 10,
                                              LLM_model = "gpt-4o",
                                              max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  testthat::expect_error(theoraizer::var_list(topic = "Climate Change",
                                              n_final = 5,
                                              n_variables = 10,
                                              LLM_model = "gpt-3.5-turbo",
                                              max_tokens = "4000"),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})
