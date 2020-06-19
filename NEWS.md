# card (development version)

## Next Steps

## Bugs

* `cosinor()` unable to run on certain models based on y values

## Features

* `cosinor_features()` allows for assessing global/special attributes of multiple component cosinor analysis
* `ggcosinor()` is now functional for single and multiple component analysis
* Sequential model building can be performed with `build_sequential_models()`, however it is in a list format and will likely be updated to be more "tidy" in the future
* Confidence interval methods now work for population-mean cosinor, including summary function
* `ggpopcosinor()` can show the cosinors for individuals across a population, along with mean and predicted cosinor
* `ggcosinor()` accepts single models
* `print.cosinor()` and `plot.cosinor()` functions added
* `cosinor_zero_amplitude()` test added, works for individual cosinor.
* Population-mean cosinor analysis is added. `cosinor()` now takes the argument
of for individuals. The individual cosinor methods generally work, but may not
yet be accurate.
* Circadian rhythm analysis has also created an initial family of functions that
will work to simplify the process of analyzing 24-hour data. The
`circ_compare_groups()` helps to summarize circadian data by an covariate and
time. This is visualized using `ggcircadian()`. Also includes the `ggforest()`
to create forest plots of odds ratios. This is dependent on the `circ_odds()`
function to generate odds ratios by time.
* An important regression function, built with the _tidymodels_
[hardhat](https://tidymodels.github.io/hardhat/) package, `cosinor()` introduced
as a new function to allow for diagnostic analysis of circadian patterns.
Although the algorithm is well known, having an implementation in R allows
potential diagnostics. This includes the `ggcosinorfit()` allows for assessing
rhythmicity and confidence intervals of amplitude and acrophase of cosinor
model. Basic methods for assessing the model, such as `print`, `summary`,
`coef`, and `confint` currently function.
* Recurrent events can now be analyzed using a powerful function called
`recur_survival_table()`, which allows for redesigning longitudinal data tables
into a model appropriate for analysis. It is built to extend survival analyses.
The `recur_summary_table()` function allows for reviewing the findings from
recurrent events by category to help understand event strata.
* The `circ_sun()` function allows for identifying the sunrise and sunset times
based on geographical location. This is intended to couple with the
`circ_center()` function to center a time series around an event, such as
sunrise. A vignette has been added to review this data.

