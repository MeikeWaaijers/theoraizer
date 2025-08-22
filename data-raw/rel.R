rel <- causal_relation(topic = "Addiction",
                       variable_list = vars$final_list,
                       LLM_model = "gpt-4o",
                       max_tokens = 2000,
                       update_key = FALSE)

usethis::use_data(rel, overwrite = TRUE)
