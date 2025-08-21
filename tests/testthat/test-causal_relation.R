library(testthat)
library(vcr)
library(theoraizer)

test_that("causal_relation() works", {
  skip_on_cran()
  vcr::use_cassette("causal_relation", {
    rel <- theoraizer::causal_relation(topic = "addiction",
                                       variable_list = theoraizer::vars$final_list[1:2])

  })
  testthat::expect_true(!is.null(rel))
  testthat::expect_true(is.list(rel))
  testthat::expect_true(all(c("raw_LLM",
                              "relation_df") %in% names(rel)))
  testthat::expect_true(is.data.frame(rel$raw_LLM))
  testthat::expect_true(all(c("relationship",
                              "iteration",
                              "LLM_model",
                              "prompt",
                              "system_prompt",
                              "content",
                              "finish_reason",
                              "prompt_tokens",
                              "answer_tokens",
                              "total_tokens",
                              "error") %in% names(rel$raw_LLM)))
  testthat::expect_true(is.data.frame(rel$relation_df))
  testthat::expect_true(all(c("var1",
                              "var2",
                              "prob_causal") %in% names(rel$relation_df)))
})


test_that("Invalid topic parameter causes error", {
  testthat::expect_error(theoraizer::causal_relation(topic = 123,
                                                     variable_list = c("CO2", "Temperature", "Sea Level"),
                                                     LLM_model = "gpt-4",
                                                     max_tokens = 1000),
                         "'topic' should be a single non-empty character string or NULL.")
})

test_that("Invalid variable_list parameter causes error", {
  testthat::expect_error(theoraizer::causal_relation(topic = "Climate Change",
                                                     variable_list = "not_a_vector",
                                                     LLM_model = "gpt-4",
                                                     max_tokens = 1000),
                         "'variable_list' should be a vector containing more than one variables.")
})

test_that("Invalid entries in variable_list cause error", {
  testthat::expect_error(theoraizer::causal_relation(topic = "Climate Change",
                                                     variable_list = list(123, "Temperature", "Sea Level"),
                                                     LLM_model = "gpt-4",
                                                     max_tokens = 1000),
                         "All entries in 'variable_list' should be character strings.")
})

test_that("Invalid LLM_model parameter causes error", {
  testthat::expect_error(theoraizer::causal_relation(topic = "Climate Change",
                                                     variable_list = c("CO2", "Temperature", "Sea Level"),
                                                     LLM_model = "invalid_model",
                                                     max_tokens = 1000),
                         "'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', 'mixtral', or 'llama-3'.")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  testthat::expect_error(theoraizer::causal_relation(topic = "Climate Change",
                                                     variable_list = c("CO2", "Temperature", "Sea Level"),
                                                     LLM_model = "gpt-4o",
                                                     max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  testthat::expect_error(theoraizer::causal_relation(topic = "Climate Change",
                                                     variable_list = c("CO2", "Temperature", "Sea Level"),
                                                     LLM_model = "gpt-3.5-turbo",
                                                     max_tokens = "4000"),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})
