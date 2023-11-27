# Calculate the rolling returns
comparison_data_rr <- comparison_data %>%
    mutate(asisa_rollrets = RcppRoll::roll_prod(1 + mean_asisa_ret, 36, fill = NA, align = "right")^(12/36) - 1,
           ai_rollrets = RcppRoll::roll_prod(1 + ai_returns, 36, fill = NA, align = "right")^(12/36) - 1,
           bm_rollrets = RcppRoll::roll_prod(1 + bm_returns, 36, fill = NA, align = "right")^(12/36) - 1) %>%
    group_by(date) %>%
    filter(any(!is.na(asisa_rollrets)) | any(!is.na(ai_rollrets)) | any(!is.na(bm_rollrets))) %>%
    ungroup()

# Create the plot
g <- ggplot(comparison_data_rr, aes(x = date)) +
    geom_line(aes(y = asisa_rollrets, color = "ASISA Mean"), alpha = 0.7, size = 1.25) +
    geom_line(aes(y = ai_rollrets, color = "AI Fund"), alpha = 0.7, size = 1.25) +
    geom_line(aes(y = bm_rollrets, color = "Benchmark"), alpha = 0.7, size = 1.25) +
    labs(title = "3-Year Annualized Rolling Returns",
         subtitle = "Comparison of ASISA Mean, AI Fund, and Benchmark",
         x = "",
         y = "Rolling 3-Year Returns (Annualized)",
         caption = "Note: Active fund fees not included") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal() +
    theme(legend.title = element_blank())

# fmxdat package for theming and final touches
if(requireNamespace("fmxdat", quietly = TRUE)) {
    g <- g + fmxdat::theme_fmx(title.size = 30, subtitle.size = 5, caption.size = 25, CustomCaption = TRUE) +
        fmxdat::fmx_cols()

    # Final touches using finplot
    g <- fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = TRUE, y.pct = TRUE, y.pct_acc = 1)
}


