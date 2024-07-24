library(testthat)
library(vcr)
library(theoraizer)

test_that("cld() works", {
  skip_on_cran()
  vcr::use_cassette("cld", {
    output <- theoraizer::cld(topic = "addiction",
                              variable_list = theoraizer::vars$final_list[1:2],
                              plot = TRUE,
                              LLM_model = "gpt-4",
                              max_tokens = 1000)
  })
  testthat::expect_true(!is.null(output))
  testthat::expect_true(is.list(output))
  testthat::expect_true(all(c("raw_LLM",
                              "sign_df") %in% names(output)))
  testthat::expect_true(is.data.frame(output$raw_LLM))
  testthat::expect_true(all(c("which_fun",
                              "var",
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
                              "error") %in% names(output$raw_LLM)))
  testthat::expect_true(is.data.frame(output$sign_df))
  testthat::expect_true(all(c("var1",
                              "var2",
                              "prob_causal",
                              "prob_var1_cause",
                              "prob_var1_pos",
                              "prob_var1_neg",
                              "prob_var2_cause",
                              "prob_var2_pos",
                              "prob_var2_neg") %in% names(output$sign_df)))

})

test_that("Invalid variable_list parameter causes error", {
  testthat::expect_error(theoraizer::cld(topic = "Climate Change",
                                         variable_list = "not_a_vector",
                                         plot = TRUE,
                                         LLM_model = "gpt-4",
                                         max_tokens = 1000),
                         "variable_list' should be a vector containing more than one variables.")
})

test_that("Invalid entries in variable_list cause error", {
  testthat::expect_error(theoraizer::cld(topic = "Climate Change",
                                         variable_list = list(123, "Temperature", "Sea Level"),
                                         plot = TRUE,
                                         LLM_model = "gpt-4",
                                         max_tokens = 1000),
                         "All entries in 'variable_list' should be character strings.")
})

test_that("Invalid plot parameter causes error", {
  testthat::expect_error(theoraizer::cld(topic = "Climate Change",
                                         variable_list = c("CO2", "Temperature", "Sea Level"),
                                         plot = "not_logical",
                                         LLM_model = "gpt-4",
                                         max_tokens = 1000),
                         "'plot' should be a logical value.")
})

test_that("Invalid LLM_model parameter causes error", {
  testthat::expect_error(theoraizer::cld(topic = "Climate Change",
                                         variable_list = c("CO2", "Temperature", "Sea Level"),
                                         plot = TRUE,
                                         LLM_model = "invalid_model",
                                         max_tokens = 1000),
                         "'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', or 'mixtral'.")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  testthat::expect_error(theoraizer::cld(topic = "Climate Change",
                                         variable_list = c("CO2", "Temperature", "Sea Level"),
                                         plot = TRUE,
                                         LLM_model = "gpt-4o",
                                         max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  testthat::expect_error(theoraizer::cld(topic = "Climate Change",
                                         variable_list = c("CO2", "Temperature", "Sea Level"),
                                         plot = TRUE,
                                         LLM_model = "gpt-3.5-turbo",
                                         max_tokens = "4000"),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})
