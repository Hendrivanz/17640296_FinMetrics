# large cap df (Inefficient I know)
large_cap_df <- ALSI %>%  filter(Index_Name == "Large_Caps") %>%
    mutate(J203_returns = J203*Return, J403_returns = J403*Return) %>%
    mutate(J203_cum_returns = cumprod(1 + J203_returns),
           J403_cum_returns = cumprod(1 + J403_returns))

# Cum return plot for large caps
large_cum_plot <- large_cap_df %>%
    select(date, J403_cum_returns, J203_cum_returns) %>%
    gather(Index, cum_returns, -date ) %>%
    ggplot() + geom_line(aes(date, cum_returns, color = Index)) +
    theme_bw() + labs(title = "ALSI and SWIX cumulative returns on Large Caps",
                      y = "Cumulative returns" )

# Mid cap df
mid_cap_df <- ALSI %>% filter(Index_Name == "Mid_Caps") %>%
    mutate(J203_returns = J203*Return, J403_returns = J403*Return) %>%
    mutate(J203_cum_returns = cumprod(1 + J203_returns),
           J403_cum_returns = cumprod(1 + J403_returns))

# Cum return plot for mid caps
mid_cum_plot <- large_cap_df %>%
    select(date, J403_cum_returns, J203_cum_returns) %>%
    gather(Index, cum_returns, -date ) %>%
    ggplot() + geom_line(aes(date, cum_returns, color = Index)) +
    theme_bw() + labs(title = "ALSI and SWIX cumulative returns on Mid Caps",
                      y = "Cumulative returns" )

# small caps df
small_cap_df <- ALSI %>% filter(Index_Name == "Small_Caps") %>%
    mutate(J203_returns = J203*Return, J403_returns = J403*Return) %>%
    mutate(J203_cum_returns = cumprod(1 + J203_returns),
           J403_cum_returns = cumprod(1 + J403_returns))

# Small caps cumulative return plot
small_cum_plot <- large_cap_df %>% select(date, J403_cum_returns, J203_cum_returns) %>%
    gather(Index, cum_returns, -date ) %>%
    ggplot() + geom_line(aes(date, cum_returns, color = Index)) +
    theme_bw() + labs(title = "ALSI and SWIX cumulative returns on Small Caps",
                      y = "Cumulative returns" )