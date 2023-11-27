
# calc cum returns for all instead of by cap_size
q3_df <-  ALSI  %>% mutate(J203_returns = J203*Return, J403_returns = J403*Return) %>%
    mutate(J203_cum_returns = cumprod(1 + J203_returns),
           J403_cum_returns = cumprod(1 + J403_returns))

# Plot cumulative returns by sector
sectors_plot <- q3_df %>%
    pivot_longer(cols = c(J203_cum_returns, J403_cum_returns),
                 names_to = "Index",
                 values_to = "Cum_Returns") %>%
    ggplot(aes(x = date, y = Cum_Returns, color = Index)) +
    geom_line() +
    theme_bw() +
    labs(title = "Cumulative returns by Sector",
         x = "Date",
         y = "Cumulative Returns") +
    facet_wrap(~Sector, scales = "free_y") # Facet by Sector