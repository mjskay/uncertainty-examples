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
```

## Data

We’ll use this data on US unemployment rates:

``` r
df = read_csv("us-unemployment.csv", col_types = cols(
    date = col_date(), 
    unemployment = col_double()
  )) %>%
  mutate(
    m = month(date),
    time = 1:n()
  )
```

Which looks like this:

``` r
df %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line() +
  ylim(0, NA) +
  coord_cartesian(expand = FALSE)
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Model

We’ll fit a relatively simple time series model using `bsts` (Bayesian
Structural Time Series). I wouldn’t use this model for anything
important—there isn’t really any domain knowledge going into this, I’m
not a time series expert nor am I an expert in unemployment.

``` r
set.seed(123456)
m = with(df, bsts(unemployment, state.specification = list() %>%
    AddSemilocalLinearTrend(unemployment) %>%
    AddSeasonal(unemployment, 12),
  niter = 10000))
```

    ## =-=-=-=-= Iteration 0 Sat Feb 16 16:07:50 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 1000 Sat Feb 16 16:08:00 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 2000 Sat Feb 16 16:08:10 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 3000 Sat Feb 16 16:08:19 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 4000 Sat Feb 16 16:08:28 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 5000 Sat Feb 16 16:08:38 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 6000 Sat Feb 16 16:08:47 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 7000 Sat Feb 16 16:08:56 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 8000 Sat Feb 16 16:09:05 2019
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 9000 Sat Feb 16 16:09:14 2019
    ##  =-=-=-=-=

## Spaghetti plot

We’ll start by pulling out the fits for the existing data and some
predictions for the next year:

``` r
forecast_months = 13   # number of months forward to forecast
set.seed(123456)

fits = df %>%
  add_draws(colSums(aperm(m$state.contributions, c(2, 1, 3))))

predictions = df %$%
  tibble(
    date = max(date) + months(1:forecast_months),
    m = month(date),
    time = max(time) + 1:forecast_months
  ) %>%
  add_draws(predict(m, horizon = forecast_months)$distribution, value = ".prediction")

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
  ylim(0, NA) +
  coord_cartesian(expand = FALSE)
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This is pretty hard to see, so let’s just look at the data since 2012:

``` r
since_year = 2012
set.seed(123456)
fit_color = "#3573b9"
prediction_color = "#e41a1c"

df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  geom_line(aes(y = .value, group = .draw), alpha = 1/30, color = fit_color, size = .75,
    data = fits %>% filter(year(date) >= since_year) %>% sample_draws(100)) +
  geom_line(aes(y = .prediction, group = .draw), alpha = 1/20, color = prediction_color, size = .75,
    data = predictions %>% sample_draws(100)) +
  geom_point(size = 0.75) +
  ylim(0, NA) +
  coord_cartesian(expand = FALSE)
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Uncertainty bands

We could instead use predictive bands:

``` r
df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  stat_lineribbon(aes(y = .value), fill = fit_color, color = fit_color, alpha = 1/5, data = fits %>% filter(year(date) >= since_year)) +
  stat_lineribbon(aes(y = .prediction), fill = prediction_color, color = prediction_color, alpha = 1/5, data = predictions) +
  geom_point(size = 0.75) +
  ylim(0, NA) +
  coord_cartesian(expand = FALSE)
```

    ## Warning: Removed 6 rows containing non-finite values (stat_pointinterval).

![](us-unemployment_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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
  ylim(0, NA) +
  coord_cartesian(expand = FALSE)
```

    ## Warning: Removed 6 rows containing non-finite values (stat_pointinterval).

![](us-unemployment_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Density plot

``` r
y_max = 9

fit_plot = df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  stat_lineribbon(aes(y = .value), fill = fit_color, alpha = 1/n_bands, .width = ppoints(n_bands),
    data = fits %>% filter(year(date) >= since_year), color = NA) +
  geom_point(size = 0.75) +
  ylim(0, y_max) +
  coord_cartesian(expand = FALSE)

predict_plot = predictions %>%
  filter(date %in% c(ymd("2019-02-01"), ymd("2019-08-01"), ymd("2020-02-01"))) %>%
  ggplot(aes(x = .prediction)) +
  stat_density(fill = prediction_color, adjust = 2, alpha = 3/5) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL, limits = c(0, y_max)) +
  coord_flip(expand = FALSE) +
  facet_grid(. ~ date, labeller = labeller(date = function(x) strftime(x, "%b\n%Y")), switch = "x") +
  theme(strip.text.x = element_text(hjust = 0, size = 8))

plot_grid(align = "h", axis = "tb", ncol = 2, rel_widths = c(3, 1),
  fit_plot,
  predict_plot
  )
```

    ## Warning: Removed 6 rows containing non-finite values (stat_density).

![](us-unemployment_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Can’t decide if I prefer the density normalized within predicted month
or not:

``` r
fit_plot = df %>%
  filter(year(date) >= since_year) %>%
  ggplot(aes(x = date, y = unemployment)) +
  stat_lineribbon(aes(y = .value), fill = fit_color, alpha = 1/n_bands, .width = ppoints(n_bands),
    data = fits %>% filter(year(date) >= since_year), color = NA) +
  geom_point(size = 0.75) +
  ylim(0, y_max) +
  coord_cartesian(expand = FALSE)

predict_plot = predictions %>%
  filter(date %in% c(ymd("2019-02-01"), ymd("2019-08-01"), ymd("2020-02-01"))) %>%
  ggplot(aes(x = .prediction)) +
  stat_density(fill = prediction_color, adjust = 2, alpha = 3/5) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL, limits = c(0, y_max)) +
  coord_flip(expand = FALSE) +
  facet_grid(. ~ date, labeller = labeller(date = function(x) strftime(x, "%b\n%Y")), switch = "x", scales = "free_x") +
  theme(strip.text.x = element_text(hjust = 0, size = 8))

plot_grid(align = "h", axis = "tb", ncol = 2, rel_widths = c(3, 1),
  fit_plot,
  predict_plot
  )
```

    ## Warning: Removed 6 rows containing non-finite values (stat_density).

![](us-unemployment_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Quantile dotplot

``` r
predict_plot = predictions %>%
  filter(date %in% c(ymd("2019-02-01"), ymd("2019-08-01"), ymd("2020-02-01"))) %>%
  group_by(date) %>%
  do(tibble(.prediction = quantile(.$.prediction, ppoints(50)))) %>%
  ggplot(aes(x = .prediction)) +
  geom_dotplot(fill = prediction_color, color = NA, binwidth = .1, alpha = 3/5) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL, limits = c(0, y_max)) +
  coord_flip(expand = FALSE) +
  facet_grid(. ~ date, labeller = labeller(date = function(x) strftime(x, "%b\n%Y")), switch = "x", scales = "free_x") +
  theme(strip.text.x = element_text(hjust = 0, size = 8))

plot_grid(align = "h", axis = "tb", ncol = 2, rel_widths = c(3, 1),
  fit_plot,
  predict_plot
  )
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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
  ylim(0, NA) +
  coord_cartesian(expand = FALSE) +
  transition_states(.draw, 0, 1) 

animate(anim, nframes = n_frames, fps = n_frames / n_hops * 2.5, res = 100, width = 600, height = 400, type = "cairo")
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-13-1.gif)<!-- -->

Or HOPs with static ensemble in the background:

``` r
anim = anim +
  shadow_mark(past = TRUE, future = TRUE, color = "black", alpha = 1/50)

animate(anim, nframes = n_frames, fps = n_frames / n_hops * 2.5, res = 100, width = 600, height = 400, type = "cairo")
```

![](us-unemployment_files/figure-gfm/unnamed-chunk-14-1.gif)<!-- -->
