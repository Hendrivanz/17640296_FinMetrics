# Compute mean returns for the active funds at each date
asisa_meds <- ASISA_active %>%
    group_by(date) %>%
    summarize(med_asisa_ret = median(Returns))

ASISA_means <- ASISA_active %>%
    group_by(date) %>%
    summarize(mean_asisa_ret = mean(Returns))

# Merge the mean returns with AI fund and benchmark data
comparison_data_combine <- ASISA_means %>%
    left_join(ai_fund, by = "date") %>%
    left_join(bm, by = "date") %>%
    left_join(asisa_meds, by = "date")

# Calculate cumulative returns
comparison_data <- comparison_data_combine %>%
    mutate(cumulative_return_asisa = cumprod(1 + mean_asisa_ret) - 1,
           cumulative_return_ai = cumprod(1 + ai_returns) - 1,
           cumulative_return_bm = cumprod(1 + bm_returns) - 1,
           cumulative_return_asisa_med = cumprod(1 + med_asisa_ret)-1)

# Visualize cumulative returns
q1_cumplot <- ggplot(comparison_data, aes(x = date)) +
    geom_line(aes(y = cumulative_return_asisa, color = "ASISA Mean")) +
    geom_line(aes(y = cumulative_return_ai, color = "AI Fund")) +
    geom_line(aes(y = cumulative_return_bm, color = "Benchmark")) +
    geom_line(aes(y=cumulative_return_asisa_med, color = "ASISA Median")) +
    labs(title = "AI Fund vs Benchmark vs Active Fund (Mean and Median)",
         x = "Date",
         y = "Cumulative Return",
         color = "Legend") +
    theme_classic()