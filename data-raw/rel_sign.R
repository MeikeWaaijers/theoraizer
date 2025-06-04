rel_sign <- causal_sign(context = "Addiction",
                        prob_df = rel$relation_df,
                        causal_threshold = 50,
                        LLM_model = "gpt-4o",
                        max_tokens = 2000,
                        update_key = FALSE)

usethis::use_data(rel_sign, overwrite = TRUE)
