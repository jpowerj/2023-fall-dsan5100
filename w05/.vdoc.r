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
#| label: random-walk-16
library(tibble)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(tidyr)
# From McElreath!
gen_histo <- function(reps, num_steps) {
  support <- c(-1,1)
  pos <-replicate(reps, sum(sample(support,num_steps,replace=TRUE,prob=c(0.5,0.5))))
  #print(mean(pos))
  #print(var(pos))
  pos_df <- tibble(x=pos)
  clt_distr <- function(x) dnorm(x, 0, sqrt(num_steps))
  plot <- ggplot(pos_df, aes(x=x)) +
    geom_histogram(aes(y = after_stat(density)), fill=cbPalette[1], binwidth = 2) +
    stat_function(fun = clt_distr) +
    dsan_theme("quarter") +
    theme(title=element_text(size=16)) +
    labs(
      title=paste0(reps," Random Walks, ",num_steps," Steps")
    )
  return(plot)
}
gen_walkplot <- function(num_people, num_steps, opacity=0.15) {
  support <- c(-1, 1)
  # Unique id for each person
  pid <- seq(1, num_people)
  pid_tib <- tibble(pid)
  pos_df <- tibble()
  end_df <- tibble()
  all_steps <- t(replicate(num_people, sample(support, num_steps, replace = TRUE, prob = c(0.5, 0.5))))
  csums <- t(apply(all_steps, 1, cumsum))
  csums <- cbind(0, csums)
  # Last col is the ending positions
  ending_pos <- csums[, dim(csums)[2]]
  end_tib <- tibble(pid = seq(1, num_people), endpos = ending_pos, x = num_steps)
  # Now convert to tibble
  ctib <- as_tibble(csums, name_repair = "none")
  merged_tib <- bind_cols(pid_tib, ctib)
  long_tib <- merged_tib %>% pivot_longer(!pid)
  # Convert name -> step_num
  long_tib <- long_tib %>% mutate(step_num = strtoi(gsub("V", "", name)) - 1)
  # print(end_df)
  grid_color <- rgb(0, 0, 0, 0.1)

  # And plot!
  walkplot <- ggplot(
      long_tib,
      aes(
          x = step_num,
          y = value,
          group = pid,
          # color=factor(label)
      )
  ) +
      geom_line(linewidth = g_linesize, alpha = opacity, color = cbPalette[1]) +
      geom_point(data = end_tib, aes(x = x, y = endpos), alpha = 0) +
      scale_x_continuous(breaks = seq(0, num_steps, num_steps / 4)) +
      scale_y_continuous(breaks = seq(-20, 20, 10)) +
      dsan_theme("quarter") +
      theme(
          legend.position = "none",
          title = element_text(size = 16)
      ) +
      theme(
          panel.grid.major.y = element_line(color = grid_color, linewidth = 1, linetype = 1)
      ) +
      labs(
          title = paste0(num_people, " Random Walks, ", num_steps, " Steps"),
          x = "Number of Steps",
          y = "Position"
      )
}
wp1 <- gen_walkplot(500, 16, 0.05)
ggMarginal(wp1, margins = "y", type = "histogram", yparams = list(binwidth = 1))
#
#
#
#
#
#| label: random-walk-64
library(ggExtra)
wp2 <- gen_walkplot(5000,64,0.008) +
  ylim(-30,30)
ggMarginal(wp2, margins = "y", type = "histogram", yparams = list(binwidth = 1))
#
#
#
#
#
#
#
#| fig-height: 3.4
#| label: walk-histo-16
p2 <- gen_histo(1000, 16)
p2
#
#
#
#| fig-height: 3.4
#| label: walk-histo-32
p3 <- gen_histo(10000, 32)
p3
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
#| label: discrete-uniform-pmf
#| echo: true
#| code-fold: true
#| fig-align: center
library(tibble)
bar_data <- tribble(
  ~x, ~prob,
  1, 1/6,
  2, 1/6,
  3, 1/6,
  4, 1/6,
  5, 1/6,
  6, 1/6
)
ggplot(bar_data, aes(x=x, y=prob)) +
  geom_bar(stat="identity", fill=cbPalette[1]) +
  labs(
    title="Discrete Uniform pmf: a = 1, b = 6",
    y="Probability Mass",
    x="Value"
  ) +
  scale_x_continuous(breaks=seq(1,6)) +
  dsan_theme("half")
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
#| label: geometric-demo
#| fig-align: center
#| echo: true
#| code-fold: true
library(ggplot2)
k <- seq(0, 8)
prob <- dgeom(k, 0.5)
bar_data <- tibble(k, prob)
ggplot(bar_data, aes(x = k, y = prob)) +
    geom_bar(stat = "identity", fill = cbPalette[1]) +
    labs(
        title = "Geometric Distribution, p = 0.5",
        y = "Probability Mass"
    ) +
    scale_x_continuous(breaks = seq(0, 8)) +
    dsan_theme("half")
#
#
#
#
#
#| label: exponential-plot
#| echo: true
#| code-fold: true
#| fig-align: center
my_dexp <- function(x) dexp(x, rate = 1/2)
ggplot(data.frame(x=c(0,8)), aes(x=x)) +
  stat_function(fun=my_dexp, size=g_linesize, fill=cbPalette[1], alpha=0.8) +
  stat_function(fun=my_dexp, geom='area', fill=cbPalette[1], alpha=0.75) +
  dsan_theme("half") +
  labs(
    title="Exponential Distribution PDF, lambda = 0.5",
    x = "v",
    y = "f_X(v)"
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
#| label: cauchy-pdf
#| echo: true
#| code-fold: true
ggplot(data.frame(x=c(-4,4)), aes(x=x)) +
  stat_function(fun=dcauchy, size=g_linesize, fill=cbPalette[1], alpha=0.8) +
  stat_function(fun=dcauchy, geom='area', fill=cbPalette[1], alpha=0.5) +
  dsan_theme("quarter") +
  labs(
    title="PDF of R",
    x = "r",
    y = "f(r)"
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
