cld_example <- cld(context = "Addiction",
                   variable_list = vars$final_list,
                   plot = TRUE,
                   LLM_model = "gpt-4o",
                   max_tokens = 2000,
                   update_key = FALSE)

usethis::use_data(cld_example, overwrite = TRUE)
