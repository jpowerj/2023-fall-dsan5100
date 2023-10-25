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
#| label: r-source-globals
source("../_globals.r")
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
#| label: overfitting-plot
x <- seq(from = 0, to = 1, by = 0.1)
n <- length(x)
eps <- rnorm(n, 0, 0.04)
y <- x + eps
# But make one big outlier
midpoint <- ceiling((3/4)*n)
y[midpoint] <- 0
of_data <- tibble::tibble(x=x, y=y)
# Linear model
lin_model <- lm(y ~ x)
# But now polynomial regression
poly_model <- lm(y ~ poly(x, degree = 10, raw=TRUE))
ggplot(of_data, aes(x = x, y = y)) +
    geom_point(size = g_pointsize / 1.5) +
    dsan_theme("full")
#
#
#
#
#
#
#
#| label: of-linear
ggplot(of_data, aes(x = x, y = y)) +
    geom_point(size = g_pointsize / 1.5) +
    geom_smooth(aes(color="Linear"), method = lm, se = FALSE, show.legend=FALSE) +
    # geom_abline(aes(intercept = 0, slope = 1, color = "Linear"), linewidth = 1, show.legend = FALSE) +
    # stat_smooth(
    #     method = "lm",
    #     formula = y ~ poly(x, 10, raw = TRUE),
    #     se = FALSE, aes(color = "Polynomial")
    # ) +
    dsan_theme("full")
#
#
#
#
#
#
#
#
#
#| label: setup-sup-data
## Part 1: Set up 
sup_data <- tibble::tribble(
  ~home_id, ~sqft, ~bedrooms, ~rating,
  0, 1000, 1, "Disliked",
  1, 2000, 2, "Liked",
  2, 2500, 1, "Liked",
  3, 1500, 2, "Disliked",
  4, 2200, 1, "Liked"
)
x_min <- 0
x_max <- 3000
y_min <- -1
y_max <- 3
rand_y0 <- runif(50, min = y_min, max = y_max)
rand_y1 <- runif(50, min = y_min, max = y_max)
rand_slope <- (rand_y1 - rand_y0) / (x_max - x_min)
rand_intercept <- rand_y0
rand_lines <- tibble::tibble(id = 1:50, slope = rand_slope, intercept = rand_intercept)
#
#
#
#| label: bootstrap-models
library(dplyr)
library(ggplot2)
library(tibble)
# subsample <- of_data |> sample_n() sample(of_data, size=5)
gen_subsamples <- function(obs_data, num_subsamples, subsample_size) {
  #print(subsample_size)
  subsample_ints <- c()
  subsample_coefs <- c()
  for (i in 1:num_subsamples) {
      cur_subsample <- obs_data |> sample_n(subsample_size, replace = TRUE)
      cur_lin_model <- lm(y ~ x, data = cur_subsample)
      cur_int <- cur_lin_model$coefficients[1]
      subsample_ints <- c(subsample_ints, cur_int)
      cur_coef <- cur_lin_model$coefficients[2]
      subsample_coefs <- c(subsample_coefs, cur_coef)
  }
  subsample_df <- tibble(intercept = subsample_ints, coef = subsample_coefs)
  return(subsample_df)
}
num_subsamples <- 50
subsample_size <- floor(nrow(of_data) / 2)
subsample_df <- gen_subsamples(of_data, num_subsamples, subsample_size)
full_model <- lm(y ~ x, data = of_data)
full_int <- full_model$coefficients[1]
full_coef <- full_model$coefficients[2]
full_df <- tibble(intercept=full_int, coef=full_coef)
mean_df <- tibble(
    intercept=mean(subsample_df$intercept),
    coef = mean(subsample_df$coef)
)
#
#
#
ggplot(of_data, aes(x = x, y = y)) +
    geom_point(size=g_pointsize) +
    # The random lines
    geom_abline(data = subsample_df, aes(slope = coef, intercept = intercept, color='Subsample Model'), linewidth=g_linewidth, linetype="solid", alpha=0.25) +
    # The original regression line
    geom_abline(data=full_df, aes(slope = coef, intercept = intercept, color='Full-Data Model'), linewidth=2*g_linewidth) +
    # The average of the random lines
    #geom_abline(data=mean_df, aes(slope = coef, intercept = intercept, color='mean'), linewidth=2*g_linewidth) +
    labs(
        title = paste0("Linear Models for ", num_subsamples, " Subsamples of Size n = ", subsample_size),
        color = element_blank()
    ) +
    dsan_theme("full") +
    theme(
      legend.title = element_blank(),
      legend.spacing.y = unit(0, "mm")
    )
#
#
#
#
#
#| label: robust-plot
x <- seq(from = 0, to = 1, by = 0.1)
n <- length(x)
eps <- rnorm(n, 0, 0.04)
y <- x + eps
robust_data <- tibble(x = x, y = y)
robust_sub_df <- gen_subsamples(robust_data, 30, 5)
#print(robust_sub_df)
full_model_robust <- lm(y ~ x, data = robust_data)
full_int_robust <- full_model_robust$coefficients[1]
full_coef_robust <- full_model_robust$coefficients[2]
full_df_robust <- tibble(intercept = full_int_robust, coef = full_coef_robust)
ggplot(robust_data, aes(x = x, y = y)) +
    geom_point(size=g_pointsize) +
    # The random lines
    geom_abline(data = robust_sub_df, aes(slope = coef, intercept = intercept, color='Subsample Model'), linewidth=g_linewidth, linetype="solid", alpha=0.25) +
    # The original regression line
    geom_abline(data=full_df_robust, aes(slope = coef, intercept = intercept, color='Full-Data Model'), linewidth=2*g_linewidth) +
    # The average of the random lines
    #geom_abline(data=mean_df, aes(slope = coef, intercept = intercept, color='mean'), linewidth=2*g_linewidth) +
    labs(
        title = paste0("Linear Models for ", num_subsamples, " Subsamples of Size n = ", subsample_size),
        color = element_blank()
    ) +
    dsan_theme("full") +
    theme(
      legend.title = element_blank(),
      legend.spacing.y = unit(0, "mm")
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
#
#| label: random-indices
# print(sample(1:10, size=5, replace = TRUE))
# print(sample(1:10, size = 5, replace = TRUE))
# print(sample(1:10, size = 5, replace = TRUE))
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
#| label: bootstrap-accuracy
pop <- rnorm(1000000, mean = 3, sd = 1)
# Sampling 1k times
rand_samples <- replicate(
  1000,
  sample(pop, size=100, replace = FALSE)
)
sample_means <- colMeans(rand_samples)
sample_df <- tibble(est = sample_means, Method = "1000 Samples")
# Sampling 1 time and bootstrapping
bs_sample <- sample(pop, size = 100, replace = FALSE)
subsamples <- replicate(1000, sample(bs_sample, size=100, replace = TRUE))
bs_means <- colMeans(subsamples)
bs_df <- tibble(est = bs_means, Method = "Bootstrap (1 Sample)")
result_df <- bind_rows(sample_df, bs_df)
sim_dnorm <- function(x) dnorm(x, mean = 3, sd = 1)
ggplot(result_df, aes(x=est, fill=Method)) +
  dsan_theme("full") +
  geom_density(alpha=0.2, linewidth=g_linewidth) +
  geom_vline(aes(xintercept=3, linetype="value"),linewidth=g_linewidth) +
  scale_linetype_manual("", values=c("density"="solid", "value"="dashed"), labels=c("Population Mean", "testing")) +
  theme(
    legend.title = element_blank(),
    legend.spacing.y = unit(0, "mm")
  ) +
  labs(
    title = "Bootstrap vs. Multiple Samples",
    x = "Sample / Subsample Means",
    y = "Density"
  )
#
#
#
#| label: bs-estimates
sample_est <- mean(sample_means)
sample_str <- sprintf("%.3f", sample_est)
sample_err <- abs(sample_est - 3)
sample_err_str <- sprintf("%.3f", sample_err)
sample_output <- paste0("1K samples estimate: ", sample_str, " (abs. err: ", sample_err_str, ")")
writeLines(sample_output)
bs_est <- mean(bs_means)
bs_str <- sprintf("%.3f", bs_est)
bs_err <- abs(bs_est - 3)
bs_err_str <- sprintf("%.3f", bs_err)
bs_output <- paste0("Bootstrap estimate:  ", bs_str, " (abs. err: ", bs_err_str, ")")
writeLines(bs_output)
#
#
#
#
#
#
#
#
#| label: bs-intervals
#| echo: true
#| code-fold: show
quantile(bs_means, c(0.025, 0.975))
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
