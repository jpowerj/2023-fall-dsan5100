# nolint start
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: sample-space-df
source("../../_globals.r")
library(tibble)
sample_space_df <- tibble(outcome=seq(1,6))
sample_space_df
#
#
#
#
#
#| label: event-df
#| warning: false
library(dplyr)
event_df <- sample_space_df %>% filter(outcome %% 2 == 0)
event_df
#
#
#
#
#
#| label: naive-prob
nrow(event_df) / nrow(sample_space_df)
#
#
#
#
#
#
#
#
#
#| label: load-gdp-data
library(readr)
library(dplyr)
library(stringr)
gdp_df <- read_csv("https://gist.githubusercontent.com/jpowerj/fecd437b96d0954893de727383f2eaf2/raw/fec58507f7095cb8341b229d6eb74ce53232d663/gdp_2010.csv")
gdp_mean <- mean(gdp_df$value)
writeLines(paste0(gdp_mean))
below_mean_df <- gdp_df %>% filter(value < gdp_mean)
short_df <- below_mean_df %>% filter(str_detect(code, 'Z'))
short_df
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: numeric-computation
# Encode probabilities of my two events
p_greater <- 1/3
p_less_or_equal <- 2/3
# Use them to compute the probabilities of sequences
p_sequence <- p_greater * p_less_or_equal * p_greater
p_sequence
#
#
#
#
#
#
#
#
#
#| label: rigged-die
p_greater <- 0.99
p_less_or_equal <- 0.01
p_sequence <- p_greater * p_less_or_equal * p_greater
p_sequence
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: rigged-die-fn
compute_win_prob <- function(p_greater) {
    p_less_or_equal <- 1 - p_greater
    p_sequence <- p_greater * p_less_or_equal * p_greater
    return(p_sequence)
}
#
#
#
#
#
#| label: compute-win-probs
compute_win_prob(0.01)
compute_win_prob(0.5)
compute_win_prob(0.99)
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: plot-win-probs
library(ggplot2)
p_greater_vals <- seq(from = 0, to = 1, length.out = 1000)
#p_greater_vals
win_prob_vals <- sapply(p_greater_vals, compute_win_prob)
win_df <- tibble(p_greater=p_greater_vals, win_prob=win_prob_vals)
ggplot(win_df, aes(x=p_greater, y=win_prob)) +
  geom_line() +
  dsan_theme("full") +
  labs(
    title = "Probability of Winning With Rigged Die",
    x = "Pr(Result > 4)",
    y = "Pr(Win)"
  )
#
#
#
#
#
#
#
#
#
#| label: compute-maximand
max_prob <- max(win_df$win_prob)
max_row <- win_df %>% filter(win_prob == max_prob)
max_row
#
#
#
#
#
#
#
#
#
#
#
