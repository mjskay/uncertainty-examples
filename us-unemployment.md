Uncertainty examples with US unemployment data
================

## Introduction

This example shows some examples of uncertainty visualization with US
unemployment data

## Setup

``` r
library(tidyverse)
library(ggplot2)
library(rstan)
library(modelr)
library(tidybayes)
library(brms)
library(bsts)
library(gganimate)
library(cowplot)
library(lubridate)

theme_set(
  theme_tidybayes()
)

rstan_options(auto_write = TRUE)
options(mc.cores = 1)#parallel::detectCores())

knitr::opts_chunk$set(
  fig.width = 9,
  fig.height = 4.5,
  dev.args = list(type = "cairo")
)
```

## Data

We’ll use this data on US unemployment rates:

``` r
df = read_csv("us-unemployment.csv", col_types = cols(
    date = col_date(), 
    unemployment = col_double()
  )) %>%
  mutate(
    unemployment = unemployment / 100,
    logit_unemployment = qlogis(unemployment),
    m = month(date),
    time = 1:n()
  )
```

Which looks like this:

``` r
y_max = .11
y_axis = list(
  coord_cartesian(ylim = c(0, .11), expand = FALSE),
  scale_y_continuous(labels = scales::percent)
)
title = labs(x = NULL, y = NULL, subtitle = "US unemployment over time")

df %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line() +
  y_axis +
  title 
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Model

We’ll fit a relatively simple time series model using `bsts` (Bayesian
Structural Time Series). I wouldn’t use this model for anything
important—there isn’t really any domain knowledge going into this, I’m
not a time series expert nor am I an expert in unemployment.

``` r
set.seed(123456)
m = with(df, bsts(logit_unemployment, state.specification = list() %>%
    AddSemilocalLinearTrend(logit_unemployment) %>%
    AddSeasonal(logit_unemployment, 12),
  niter = 10000))
```

    ## =-=-=-=-= Iteration 0 Sat Feb 16 21:35:26 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 1000 Sat Feb 16 21:35:35 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 2000 Sat Feb 16 21:35:43 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 3000 Sat Feb 16 21:35:53 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 4000 Sat Feb 16 21:36:02 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 5000 Sat Feb 16 21:36:11 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 6000 Sat Feb 16 21:36:20 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 7000 Sat Feb 16 21:36:28 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 8000 Sat Feb 16 21:36:37 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 9000 Sat Feb 16 21:36:46 2019
    ##  =-=-=-=-=

## Spaghetti plot

We’ll start by pulling out the fits for the existing data and some
predictions for the next year:

``` r
forecast_months = 13   # number of months forward to forecast
set.seed(123456)

fits = df %>%
  add_draws(plogis(colSums(aperm(m$state.contributions, c(2, 1, 3)))))

predictions = df %$%
  tibble(
    date = max(date) + months(1:forecast_months),
    m = month(date),
    time = max(time) + 1:forecast_months
  ) %>%
  add_draws(plogis(predict(m, horizon = forecast_months)$distribution), value = ".prediction")

predictions_with_last_obs = df %>% 
  slice(n()) %>% 
  mutate(.draw = list(1:max(predictions$.draw))) %>% 
  unnest() %>% 
  mutate(.prediction = unemployment) %>% 
  bind_rows(predictions)
```

Then make a spaghetti plot:

``` r
df %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line(aes(y = .value, group = .draw), alpha = 1/20, data = fits %>% sample_draws(100)) +
  geom_line(aes(y = .prediction, group = .draw), alpha = 1/20, data = predictions %>% sample_draws(100)) +
  geom_point() +
  y_axis +
  title
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This is pretty hard to see, so let’s just look at the data since 2008:

``` r
since_year = 2008
set.seed(123456)
fit_color = "#3573b9"
prediction_color = "#e41a1c"

x_axis = scale_x_date(date_breaks = "1 years", labels = year)
arrow_spec = arrow(length = unit(6, "points"), type = "closed")
fit_annotation = list(
  annotate("text", x = ymd("2014-04-08"), y = .09, hjust = 0, vjust = 0, lineheight = 1.1,
    label = 'Uncertainty in what\nunemployment'),
  annotate("text", x = ymd("2014-04-08"), y = .09, hjust = 0, vjust = 0, lineheight = 1.1,
    label = '\n                         was', color = fit_color, fontface = "bold"),
  annotate("segment", x = ymd("2015-03-01"), xend = ymd("2015-03-01"), y = .087, yend = .06, linejoin = "mitre",
    color = fit_color, size = 1, arrow = arrow_spec)
)
prediction_annotation = list(
  annotate("text", x = ymd("2017-12-06"), y = .09, hjust = 0, vjust = 0, lineheight = 1.1,
    label = 'Uncertainty in what \nunemployment'),
  annotate("text", x = ymd("2017-12-06"), y = .09, hjust = 0, vjust = 0, lineheight = 1.1,
    label = '\n                         will be', color = prediction_color, fontface = "bold"),
  annotate("segment", x = ymd("2019-02-01"), xend = ymd("2019-02-01"), y = .087, yend = .053, linejoin = "mitre",
    color = prediction_color, size = 1, arrow = arrow_spec)
)

df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line(aes(y = .value, group = .draw), alpha = 1/30, color = fit_color, size = .75,
    data = fits %>% filter(year(date) >= since_year) %>% sample_draws(100)) +
  geom_line(aes(y = .prediction, group = .draw), alpha = 1/20, color = prediction_color, size = .75,
    data = predictions %>% sample_draws(100)) +
  geom_point(size = 0.75) +
  fit_annotation +
  prediction_annotation +
  y_axis +
  x_axis +
  title
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Uncertainty bands

We could instead use a predictive band:

``` r
df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  stat_lineribbon(aes(y = .value), fill = adjustcolor(fit_color, alpha.f = .25), color = fit_color, .width = .95,
    data = fits %>% filter(year(date) >= since_year)) +
  stat_lineribbon(aes(y = .prediction), fill = adjustcolor(prediction_color, alpha.f = .25), color = prediction_color, .width = .95,
    data = predictions) +
  geom_point(size = 0.75) +
  fit_annotation +
  prediction_annotation +
  y_axis +
  x_axis +
  title
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

But that loses a lot of nuance and fixes the reader to whatever
arbitrary interval the visualization designer chose to display. One
alternative would be to show many intervals:

``` r
df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  stat_lineribbon(aes(y = .value), fill = fit_color, color = fit_color, alpha = 1/5, data = fits %>% filter(year(date) >= since_year)) +
  stat_lineribbon(aes(y = .prediction), fill = prediction_color, color = prediction_color, alpha = 1/5, data = predictions) +
  geom_point(size = 0.75) +
  fit_annotation +
  prediction_annotation +
  y_axis +
  x_axis +
  title
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Gradient plot

Or use a large number of bands, getting us essentially a gradient plot:

``` r
n_bands = 40

df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  stat_lineribbon(aes(y = .value), fill = fit_color, alpha = 1/n_bands, .width = ppoints(n_bands), 
    data = fits %>% filter(year(date) >= since_year), color = NA) +
  stat_lineribbon(aes(y = .prediction), fill = prediction_color, alpha = 1/n_bands, .width = ppoints(n_bands),
    data = predictions, color = NA) +
  geom_point(size = 0.75) +
  fit_annotation +
  prediction_annotation +
  y_axis +
  x_axis +
  title
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Density plot

``` r
fit_plot = df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line(color = "gray75") +
  geom_point(size = 0.75) +
  y_axis +
  x_axis +
  expand_limits(x = ymd("2019-03-01")) +
  title

predict_plot = predictions %>%
  filter(date %in% c(ymd("2019-02-01"), ymd("2019-08-01"), ymd("2020-02-01"))) %>%
  ggplot(aes(x = .prediction)) +
  geom_hline(yintercept = 0, color = "gray90") +
  stat_density(fill = prediction_color, adjust = 2, alpha = 3/5) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  coord_flip(xlim = c(0, y_max), expand = FALSE) +
  facet_grid(. ~ date, labeller = labeller(date = function(x) strftime(x, "%b\n%Y")), switch = "x") +
  theme(strip.text.x = element_text(hjust = 0, size = 8))

plot_grid(align = "h", axis = "tb", ncol = 2, rel_widths = c(4, 1),
  fit_plot,
  predict_plot
  )
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Can’t decide if I prefer the density normalized within predicted month
or not:

``` r
predict_plot = predictions %>%
  filter(date %in% c(ymd("2019-02-01"), ymd("2019-08-01"), ymd("2020-02-01"))) %>%
  ggplot(aes(x = .prediction)) +
  geom_hline(yintercept = 0, color = "gray90") +
  stat_density(fill = prediction_color, adjust = 2, alpha = 3/5) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  coord_flip(xlim = c(0, y_max), expand = FALSE) +
  facet_grid(. ~ date, labeller = labeller(date = function(x) strftime(x, "%b\n%Y")), switch = "x", scale = "free_x") +
  theme(strip.text.x = element_text(hjust = 0, size = 8))

plot_grid(align = "h", axis = "tb", ncol = 2, rel_widths = c(4, 1),
  fit_plot,
  predict_plot
  )
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Quantile dotplot

``` r
predict_plot = predictions %>%
  filter(date %in% c(ymd("2019-02-01"), ymd("2019-08-01"), ymd("2020-02-01"))) %>%
  group_by(date) %>%
  do(tibble(.prediction = quantile(.$.prediction, ppoints(50)))) %>%
  ggplot(aes(x = .prediction)) +
  geom_hline(yintercept = 0, color = "gray90") +
  geom_dotplot(fill = prediction_color, color = NA, binwidth = .001, alpha = 3/5, dotsize = 1.1) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  coord_flip(xlim = c(0, y_max), expand = FALSE) +
  facet_grid(. ~ date, labeller = labeller(date = function(x) strftime(x, "%b\n%Y")), switch = "x", scales = "free_x") +
  theme(strip.text.x = element_text(hjust = 0, size = 8))

plot_grid(align = "h", axis = "tb", ncol = 2, rel_widths = c(4, 1),
    fit_plot,
    predict_plot
  ) +
  draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA)), x = .8, y = .75, width = .2, height = .2) +
  draw_label(x = 0.8, y = .9, hjust = 0, vjust = 1, size = 11, lineheight = 1.1,
    label = "50 equally likely predictions\nfor what unemployment\nin Feb 2019") +
  draw_label(x = 0.8, y = .9, hjust = 0, vjust = 1, size = 11, lineheight = 1.1,
    label = "\n\n                    will be", colour = prediction_color, fontface = "bold") +
  draw_line(x = c(0.82, 0.82), y = c(.76, .48), size = 1, colour = prediction_color, linejoin = "mitre", arrow = arrow_spec)
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## HOPs

``` r
n_hops = 100
n_frames = 100
set.seed(123456)

anim = df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line(aes(y = .prediction, group = .draw), color = prediction_color, size = .75, 
    data = predictions_with_last_obs %>% sample_draws(n_hops)) +
  geom_line(color = "gray75") +
  geom_point(size = 0.75) +
  prediction_annotation +
  y_axis +
  x_axis +
  title +
  transition_states(.draw, 0, 1) 

animate(anim, nframes = n_frames, fps = n_frames / n_hops * 2.5, res = 100, width = 900, height = 450, type = "cairo")
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-15-1.gif)<!-- -->

Or HOPs with static ensemble in the background:

``` r
anim = anim +
  shadow_mark(past = TRUE, future = TRUE, color = "black", alpha = 1/50)

animate(anim, nframes = n_frames, fps = n_frames / n_hops * 2.5, res = 100, width = 900, height = 450, type = "cairo")
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-16-1.gif)<!-- -->
