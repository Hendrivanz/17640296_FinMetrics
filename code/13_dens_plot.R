rolling_returns <- comparison_data_rr %>% select(date, asisa_rollrets, ai_rollrets, bm_rollrets)

# Calculate median rolling returns
median_bm_rollret <- median(rolling_returns$bm_rollrets, na.rm = TRUE)
median_ai_rollret <- median(rolling_returns$ai_rollrets, na.rm = TRUE)
median_asisa_rollret <- median(rolling_returns$ai_rollrets, na.rm = TRUE)

# Convert rolling returns to a long format for ggplot
rolling_returns_long <- rolling_returns %>%
    gather(key = "Fund", value = "Return", -date)

# Create the density plot
density_plot <- ggplot(rolling_returns_long, aes(x = Return, fill = Fund)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = median_bm_rollret, color = "gray", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_ai_rollret, color = "blue", linetype = "dashed", size = 1) +
    labs(title = "Density Plot of Rolling Returns",
         x = "Annualized Rolling Returns",
         y = "Density") +
    theme_minimal() +
    scale_fill_manual(values = c("blue", "orange", "grey")) +
    theme(legend.position = "bottom")  # Adjust legend position for better readability

