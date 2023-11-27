ALSI <- ALSI_df %>%  mutate_at(.vars = vars(-date), ~na.locf(., na.rm = F, maxgap = 5))
