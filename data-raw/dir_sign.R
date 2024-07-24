dir_sign <- causal_sign(topic = "addiction",
                        prob_df = dir$direction_df,
                        causal_threshold = 50,
                        LLM_model = "gpt-4o",
                        max_tokens = 2000,
                        update_key = FALSE)

usethis::use_data(dir_sign, overwrite = TRUE)
