Uncertainty Examples
================

Often, when someone asks me an uncertainty visualization question I
throw together an RMarkdown document to work through the problem. I have
started to collect some of those examples in this repository.

If you are looking for more examples, check out the
[tidybayes](http://mjskay.github.io/tidybayes/) documentation as well —
the [tidybayes+brms
vignette](http://mjskay.github.io/tidybayes/articles/tidy-brms.html) in
particular has some things not included here.

## Index

These are typically pretty roughly thrown together, and are in a bit of
a stream-of-consciousness style. You have been warned.

  - [proportions](proportions.md): demo of using hypothetical outcome
    plots (HOPs) and quantile dotplots to display uncertainty in some
    proportions

  - [linear-regression](linear-regression.md): a variety of approaches
    applied to a linear regression: HOPs, density+interval, and quantile
    dotplots of coefficients; multiple uncertainty bands, spaghetti
    plots, and HOPs for uncertainty in the fit line; and posterior
    predictive intervals for predictive uncertainty.

  - [multivariate-regression](multivariate-regression.md): a meandering
    attempt to improve on correlation heatmaps, including the use of
    gradients within density plots and a “dithering” approach.

  - [barbarella](barbarella.md): visualizations for a survey with a
    10-point rating scale analyzed with an ordinal regression model.
    Includes some posterior predictive checks, HOPs (animated
    uncertainty), quantile dotplots, and density+interval (“half-eye”)
    plots.

  - [arima](arima.md): visualizations of forecasts for a simple
    autoregressive time series model. Includes spaghetti plots and HOPs,
    plus demos the use of not-explicitly-supported packages with
    `tidybayes` (in this case, `bsts`—Bayesian structural time series).

  - [snowfall](snowfall.md): quantile dotplots and HOPs for a snowfall
    prediction.

  - [mtcars](mtcars.md): Some spaghetti plots and HOPs with mtcars data.
