library(testthat)
library(vcr)
library(theoraizer)

# Example dataframes for testing
valid_relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
invalid_relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, -70))

valid_direction_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70), prob_var1_cause = c(80, 20), prob_var2_cause = c(20, 80))

valid_rel_sign_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70), prob_pos = c(80, 20), prob_neg = c(20, 80))

valid_dir_sign_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70), prob_var1_cause = c(80, 20), prob_var1_pos = c(80, 20), prob_var1_neg = c(20, 80), prob_var2_cause = c(20, 80), prob_var2_pos = c(20, 80), prob_var2_neg = c(80, 20))


test_that("cld_plot() work correctly", {
  output <- theoraizer::cld_plot(topic = "Test Topic",
                                 relation_df = valid_relation_df,
                                 direction_df = valid_direction_df,
                                 rel_sign_df = valid_rel_sign_df,
                                 dir_sign_df = valid_dir_sign_df
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true(sum(c("rel_edge_list",
                              "dir_edge_list",
                              "rel_sign_edge_list",
                              "dir_sign_edge_list") %in% names(output)) == 4)
  testthat::expect_true(sum(c("from",
                              "to",
                              "weight") %in% names(output$rel_edge_list)) == 3)
  testthat::expect_true(sum(c("from",
                              "to",
                              "weight") %in% names(output$dir_edge_list)) == 3)
  testthat::expect_true(sum(c("from",
                              "to",
                              "weight",
                              "sign") %in% names(output$rel_sign_edge_list)) == 4)
  testthat::expect_true(sum(c("from",
                              "to",
                              "weight",
                              "sign") %in% names(output$dir_sign_edge_list)) == 4)

})


test_that("cld_plot() works with only one dataframe input", {
  output <- theoraizer::cld_plot(topic = "Test Topic",
                                 relation_df = valid_relation_df
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true("rel_edge_list" %in% names(output))
})

test_that("Invalid input causes error", {
  testthat::expect_error(theoraizer::cld_plot(topic = "Test Topic",
                                              relation_df = invalid_relation_df
  ), "All entries in 'relation_df\\$prob_causal' should be numeric and between 0 and 100.")
})

test_that("Null topic is handled", {
  output <- theoraizer::cld_plot(topic = NULL,
                                 relation_df = valid_relation_df
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true("rel_edge_list" %in% names(output))
})

test_that("Invalid plot parameter causes error", {
  testthat::expect_error(theoraizer::cld_plot(topic = "Test Topic",
                                              relation_df = valid_relation_df,
                                              plot = "not_logical"
  ), "'plot' should be a logical value.")
})

test_that("Layout parameter validation", {
  testthat::expect_error(theoraizer::cld_plot(topic = "Test Topic",
                                              relation_df = valid_relation_df,
                                              layout = "not_valid_layout"
  ), "'layout' should be either 'average' or 'circle'.")
})

test_that("Valid direction_df works correctly", {
  output <- theoraizer::cld_plot(topic = "Test Topic",
                                 direction_df = valid_direction_df
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true("dir_edge_list" %in% names(output))
})

test_that("Valid rel_sign_df works correctly", {
  output <- theoraizer::cld_plot(topic = "Test Topic",
                                 rel_sign_df = valid_rel_sign_df
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true("rel_sign_edge_list" %in% names(output))
})

test_that("Valid dir_sign_df works correctly", {
  output <- theoraizer::cld_plot(topic = "Test Topic",
                                 dir_sign_df = valid_dir_sign_df
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true("dir_sign_edge_list" %in% names(output))
})

test_that("Combine parameter is handled", {
  output <- theoraizer::cld_plot(topic = "Test Topic",
                                 direction_df = valid_direction_df,
                                 combine = FALSE
  )
  testthat::expect_true(is.list(output))
  testthat::expect_true("dir_edge_list" %in% names(output))
})
