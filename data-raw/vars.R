vars <- var_list(context = "Addiction",
                 include_context = FALSE,
                 n_final = 10,
                 n_variables = "all",
                 LLM_model = "gpt-4o",
                 max_tokens = 2000,
                 update_key = FALSE)

usethis::use_data(vars, overwrite = TRUE)
