dir <- causal_direction(topic = "Addiction",
                        relation_df = rel$relation_df,
                        causal_threshold = 50,
                        LLM_model = "gpt-4o",
                        max_tokens = 2000,
                        update_key = FALSE)

usethis::use_data(dir, overwrite = TRUE)
