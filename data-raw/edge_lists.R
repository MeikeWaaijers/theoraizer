edge_lists <- cld_plot(topic = "addiction",
                       relation_df = rel$relation_df,
                       direction_df = dir$direction_df,
                       rel_sign_df = rel_sign$sign_df,
                       dir_sign_df = cld_example$sign_df,
                       combine = TRUE,
                       relation_threshold = 50,
                       direction_threshold = 50,
                       combine_threshold = 50,
                       sign_threshold = 50,
                       plot = TRUE,
                       layout = "average",
                       legend = TRUE)

usethis::use_data(edge_lists, overwrite = TRUE)
