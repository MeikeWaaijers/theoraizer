sources <- find_source(topic = "addiction",
                       edge_list = edge_lists$dir_sign_edge_list[, 1:4],
                       causal_threshold = 50,
                       scientific = TRUE,
                       LLM_model = "gpt-4.1",
                       max_tokens = 2000,
                       update_key = FALSE)

usethis::use_data(sources, overwrite = TRUE)
