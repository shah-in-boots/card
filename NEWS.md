# card {development version}

## Updates

* The `extract_*()` echocardiogram functions are now vectorized over `text` and return one element per report. `extract_echo_findings()` accordingly returns a tibble with one row per report instead of a list.

* `extract_la_size()` gains a `range` argument for graded descriptions such as "mildly to moderately dilated", defaulting to the upper end of the range. Grades are normalized (`"mildly"` becomes `"mild"`), and `"not well seen"` is now reported as a missing size plus a separate `la_not_visualized` flag rather than being returned as if it were a size.

* `extract_lvef()` now captures values written with an inequality, such as `<20%`, and tolerates phrasing between the term and the value ("ejection fraction by visual estimate is 55%"). `extract_echo_findings()` reports the inequality in a new `lvef_qualifier` column.

* Severity and diastolic dysfunction grades in `extract_echo_findings()` are now found whether the grade precedes or follows the structure ("mild mitral regurgitation" as well as "mitral regurgitation is mild"), take the upper end of graded ranges, and match valve-inclusive terms such as "mitral valve regurgitation". The ambiguous `as` and `ar` abbreviations were dropped, as they matched ordinary English.

## Next Steps

* `cosinor()` to be expanded upon to include prediction, and integration into the __tidymodels__ approach in the `parsnip` package
	* Evaluation of plotting functions for cosinor models
	* Confidence interval methods to be improved upon

* Population cosinor analysis to be reworked for correct predictions and confidence interval estimates

## Deprecations

* The circadian-focused features are being deprecated in this upcoming release. The goal is to position functions in the appropriate package, with the key `cosinor()` functions to move to a separate package in a future release.

* The longitudinal event functions are being moved to a separate package to make maintenance more straightforward.

# card v0.1.1

## Updates

* `cosinor()` now has a stable population mean cosinor option with appropriate confidence intervals

* `procedure_codes()` has the latest ICD10 codes, as of 11/2023, and are included in the package

## Next steps

* Adding additional data analysis tools for cardiology data (including catheterization and echocardiogram data)

## Deprecations

* The circadian-rhythm features have been deprecated and recurrent data features have been removed 

* The `cosinor()` functions will be updated to be more customizable and more efficient, however will be moving to a separate package by v0.2.0

# card v0.1.0

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
* An important regression function, built with the `hardhat` package from _tidymodels_, `cosinor()` introduced
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

