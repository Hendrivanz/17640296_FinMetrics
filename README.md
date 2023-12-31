
# Financial Metrics 871 Practical Exam (17640296)

# Question 1: Systematic AI Fund

Goal is to showcase the performance of my AI_Implementer fund by
comparing it to a benchmark (Capped SWIX) and industry peers (ASISA). I
will use four objects to demonstrate performance. First, just a simple
cumulative return graph. Secondly, 3 year annualized rolling return
graph. Third, a table of statistics and fourth some distributional
results.

## Step 1: Data preprocessing

First, let’s check if there are NAs in any of the returns data.

``` r
sum(is.na(AI_Fund$AI_Fund))
```

    ## [1] 0

``` r
sum(is.na(ASISA$Returns))
```

    ## [1] 0

``` r
sum(is.na(BM$Returns))
```

    ## [1] 0

No NAs. We can proceed.

Returns recorded monthly for all data sets. This eases preprocessing. We
want to compare our fund to actively managed funds, therefore I exclude
indexes and FoF (Fund of Funds) contained in the `ASISA` df. I also
rename returns columns so they are distinct for easy identification
after joining the data frames.

``` r
ASISA_active <- ASISA %>% filter(Index == 'No', FoF == "No")  %>%  select(.,-Index, -FoF) %>% arrange(date)

bm <- BM %>% select(-Tickers) %>% rename(bm_returns = Returns) %>% arrange(date)

ai_fund <- AI_Fund %>% rename(ai_returns = AI_Fund) %>% arrange(date)
```

## Step 2: Cumulative Return graph AI fund vs Benchmark vs Active managers

In this graph I compare the performance of the AI fund against the
benchmark and the mean active managers funds.

``` r
source(paste0(path, "/code/11_cum_plot.R"))
q1_cumplot
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This came out pretty good, we can clearly see the active managers (mean
and median) have substantially lower returns than the benchmark and AI
Fund and this is without accounting for fees which would further
decrease the performance of the actively managed funds.

## Step 3: 3-year Rolling returns annualized

Annualized rolling returns are better for funds comparing over time.

``` r
source(paste0(path, "/code/12_rr_plot.R"))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning in loadfonts_win(quiet = quiet): OS is not Windows. No fonts registered
    ## with windowsFonts().

    ## Scale for y is already present.
    ## Adding another scale for y, which will replace the existing scale.

``` r
print(g)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Nice. The AI fund appears to be matching or outperforming the the
benchmark most of the time with the active fund mean yielding notably
less 3-year rolling returns (annualized).

## Step 4: Rolling standard deviation

To give us a better idea of volatility and risk

``` r
comparison_data_sd <- comparison_data %>%
  mutate(asisa_log_ret = log(1 + mean_asisa_ret),
         ai_log_ret = log(1 + ai_returns),
         bm_log_ret = log(1 + bm_returns)) %>%
 
  mutate(asisa_RollSD = roll_sd(asisa_log_ret, 36, fill = NA, align = "right") * sqrt(12),
         ai_RollSD = roll_sd(ai_log_ret, 36, fill = NA, align = "right") * sqrt(12),
         bm_RollSD = roll_sd(bm_log_ret, 36, fill = NA, align = "right") * sqrt(12)) %>%
  ungroup()

# Create the plot for rolling standard deviation
h <- ggplot(comparison_data_sd, aes(x = date)) +
  geom_line(aes(y = asisa_RollSD, color = "ASISA Mean"), alpha = 0.7, size = 1.25) +
  geom_line(aes(y = ai_RollSD, color = "AI Fund"), alpha = 0.7, size = 1.25) +
  geom_line(aes(y = bm_RollSD, color = "Benchmark"), alpha = 0.7, size = 1.25) +
  labs(title = "3-Year Annualized Rolling Standard Deviation",
       subtitle = "Comparison of ASISA Mean, AI Fund, and Benchmark",
       x = "",
       y = "Rolling 3-Year Standard Deviation (Annualized)",
       caption = "Note: Standard deviation is annualized over a rolling 3-year period.") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.title = element_blank())

# fmxdat theme stuff
if(requireNamespace("fmxdat", quietly = TRUE)) {
  h <- h + fmxdat::theme_fmx(title.size = 30, subtitle.size = 5, caption.size = 25, CustomCaption = TRUE) + 
    fmxdat::fmx_cols() 
  
  # Final touches using finplot 
  h <- fmxdat::finplot(g, x.date.dist = "1 year", x.date.type = "%Y", x.vert = TRUE, y.pct = FALSE, y.pct_acc = 1)
}
```

    ## Warning in loadfonts_win(quiet = quiet): OS is not Windows. No fonts registered
    ## with windowsFonts().

    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.

``` r
# Print the plot
print(h)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Not really surprising given that the actively managed fund is a mean
value so it will be smoother, I did not really think this one through.

## Step 5: Density plot

I wanted to get an idea of the distribution of the rolling returns for
the benchmark, AI Fund and Active fund, but it didn’t really come out
like I expected it to.

``` r
rolling_returns <- comparison_data_rr %>% select(date, asisa_rollrets, ai_rollrets, bm_rollrets)
# Convert rolling returns to a long format for ggplot
rolling_returns_long <- rolling_returns %>%
  gather(key = "Fund", value = "Return", -date)

# Create the density plot
ggplot(rolling_returns_long, aes(x = Return, fill = Fund)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Rolling Returns",
       x = "Annualized Rolling Returns",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "orange", "grey")) 
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

No idea what to make of this.

\##Step 7: Some statistical tables

Couldn’t get this to work and it’s already Sunday So I am going to move
on…

# Question 2: Currency hedging analysis

For this question I need to construct two versions of a portfolio
(hedged and unhedged) with the following allocations: 60% Equity (split
into local and global as per the 70/30 ratio) and 40% Bond (again, split
70/30).

So how I understand hedging to work in this scenario is that from the
South African perspective we may want to cancel out volatility by
cancelling our exposure to the

Check for NAs in data

``` r
sum(is.na(Indexes$date))
```

    ## [1] 0

``` r
sum(is.na(Indexes$MSCI_ACWI))
```

    ## [1] 0

``` r
sum(is.na(Indexes$Bbg_Agg))
```

    ## [1] 0

``` r
sum(is.na(Indexes$J433))
```

    ## [1] 0

``` r
sum(is.na(Indexes$ALBI))
```

    ## [1] 0

``` r
sum(is.na(ZAR$date))
```

    ## [1] 0

``` r
sum(is.na(ZAR$value))
```

    ## [1] 0

No NAs found. Next, I join the dataframes. I noticed some dates do not
overlap, for example Indexes has 2002-03-28 date, while ZAR has
2002-03-31. This suggests Indexes is recorded on the last weekday at the
end of every month, while ZAR is recorded on the last day of every
month.

## Date pre-processing

Both data sets end on 2023-08-31. ZAR starts in 1990 while Indexes
starts in 2002-02-28. I create a YM column which is use to join the two
data sets and filter for the relevant overlapping dates.

``` r
first_date <- head(Indexes$date, 1)
ZAR_clean <- ZAR  %>%  filter(date >= ymd(first_date)) %>% mutate(YM = format(date, "%Y_%B")) %>% 
    rename(ZAR.USD = value) %>% select(-Tickers)
Indexes_clean <- Indexes %>% mutate(YM = format(date,"%Y_%B" )) %>% select(-date)
q2_df <- ZAR_clean %>% left_join(Indexes_clean, by = "YM")
```

## Some initial thoughts

I understand hedging in this scenario to mean that, from a South African
perspective, we may want to reduce portfolio volatility by eliminating
exposure to the Rand (which is volatile relative to USD). So, in our
hedging portfolio we convert Rand denominated equities and bonds to
Dollar. The unhedged portfolio we leave as is.

## Step 1: Convert ZAR-denominated returns to USD for the unhedged portfolio

``` r
q2_df_unhedged <- q2_df %>%
  mutate(J433_USD = J433 / ZAR.USD,
         ALBI_USD = ALBI / ZAR.USD)
```

Ok no I had no idea what I was doing, wasted hours and couldn’t figure
it out. Panic.

# Question 3

## Step 1: Data pre-processing

check for NA’s

``` r
sum(is.na(ALSI_df$J203))
```

    ## [1] 0

``` r
sum(is.na(ALSI_df$J403))
```

    ## [1] 2

NAs present in ALSI J403. Let’s replace them with the most recently
observed past return using `~na.locf`

``` r
source(paste0(path, "/code/31_preproc.R"))
sum(is.na(ALSI$J403))
```

    ## [1] 0

Success. No more NAs in our data. We proceed.

## Compare cumulative returns by cap size

Is this the best way to test performance? No. Am I running out of time
and trying to submit at least something that showed I tried? Yes.

``` r
# note to self ALSI is J203 and SWIX is J403
source(paste0(path, "/code/32_cum_ret_plots.R"))

large_cum_plot
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> Appears as
if the performance is quite similar for between 2012 and 2020 after
which the ALSI seems to outperform the SWIX on large caps.

Compare cumulative returns on medium caps

``` r
mid_cum_plot
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Oh great, this plot looks identical to the previous one.

Compare cumulative returns on small caps

``` r
small_cum_plot
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Disappointing. The three graphs look identical…

Let’s see if we can spot any differences between sectors. I just
realized how inefficient the first steps were, but I don’t have enough
time to redo them, so I’ll do here what I should have done initially
using the `33_sectors_plot.R` script.

``` r
q3_df <-  ALSI  %>% mutate(J203_returns = J203*Return, J403_returns = J403*Return) %>% mutate(J203_cum_returns = cumprod(1 + J203_returns), J403_cum_returns = cumprod(1 + J403_returns))
```

``` r
source(paste0(path, "/code/33_sectors_plot.R"))
sectors_plot
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Wow. They’re all identical again.

Okay this has been a massive failure. I’m just curious to see whether
the weights of the various sectors in each index.

``` r
q3_df %>% mutate( Y = format(date, "%Y")) %>%  select(Y, J203_cum_returns, J403_cum_returns, Sector) %>% gather(Index, Cum_Returns, -Y, -Sector) %>% group_by(Y, Index, Sector) %>% summarise(sum(Cum_Returns)) 
```

    ## `summarise()` has grouped output by 'Y', 'Index'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 88 × 4
    ## # Groups:   Y, Index [22]
    ##    Y     Index            Sector      `sum(Cum_Returns)`
    ##    <chr> <chr>            <chr>                    <dbl>
    ##  1 2013  J203_cum_returns Financials               5214.
    ##  2 2013  J203_cum_returns Industrials             12799.
    ##  3 2013  J203_cum_returns Property                 2336.
    ##  4 2013  J203_cum_returns Resources                5795.
    ##  5 2013  J403_cum_returns Financials               5159.
    ##  6 2013  J403_cum_returns Industrials             12664.
    ##  7 2013  J403_cum_returns Property                 2311.
    ##  8 2013  J403_cum_returns Resources                5734.
    ##  9 2014  J203_cum_returns Financials              11539.
    ## 10 2014  J203_cum_returns Industrials             26095.
    ## # ℹ 78 more rows

… So much time wasted. Sorry. Ends.
