# card {development version}

## Bugs

* `cosinor_reg()` can be fitted through __parsnip__ again. The engine registration never called `parsnip::set_encoding()`, and since `parsnip::get_encoding()` returns `NULL` for a missing table rather than raising an error, the defaults it would otherwise have supplied never applied and the fit failed inside __vctrs__ with a message naming neither the model nor the engine. A specification still built and printed correctly, so nothing was visibly wrong until something was fitted with it. The registration is also no longer skipped wholesale once the model exists -- only `set_new_model()` is guarded now, the remaining setters being idempotent -- so an incomplete registration repairs itself on the next load instead of persisting for the life of the session. The documented example now fits rather than stopping at the specification, and the test for it is no longer skipped.

* `confint()` on a population-mean cosinor no longer reports amplitude and acrophase intervals that are too wide by a factor of the square root of the number of subjects. The mesor variance was divided by the subject count while the amplitude and acrophase variances were not, so the 95% interval for the amplitude of the 24-hour rhythm in `twins` came back as -0.399 to 0.975 -- an interval covering zero, and impossible values below it, for a rhythm measured across 741 subjects. It is 0.263 to 0.313.

* The acrophase standard error carried the wrong sign on its cross term. The delta method gives the amplitude a negative cross term and the acrophase a positive one, and both were written negative. The error is invisible whenever time is sampled evenly across whole cycles, since it acts only through the covariance of the two regression coefficients, which is why the bundled data never showed it; on an unevenly sampled design it inflated the standard error by around 37%.

* `cosinor_zero_amplitude()` now scales with the number of components. It fixed the numerator at 2 degrees of freedom and the denominator at `N - 3` whatever `tau` was given, so a two-component model on `twins` reported `F = 707` on 2 and 16383 degrees of freedom where the test is `F = 354` on 4 and 16381. It also returns a p-value.

* Fitted values and residuals from a population-mean cosinor are now aligned with the rows they came from. They were returned in subject order while the outcome stayed in input order, so both were correct only when the data happened to arrive sorted by subject.

* `cosinor()` now accepts non-numeric subject identifiers in `population`. The insufficient-observations filter coerced the subject names with `as.numeric()`, which made every one of them `NA` for character identifiers, so no subject was dropped and the per-subject fit then failed on a singular matrix.

* `cosinor_features()` no longer scrambles the components of a multiple-component population model when reconstructing its fitted curve, and its harmonic check now considers every period rather than only the longest and shortest, so `tau = c(24, 5, 12)` is correctly reported as non-harmonic.

## Features

* `cosinor()` objects gain the standard extractor methods -- `coef()`, `vcov()`, `sigma()`, `nobs()`, `df.residual()` and `logLik()`, and through the last of these `AIC()` and `BIC()`. Every statistic the package reports is now derived from `vcov()` rather than rebuilding the covariance matrix in each function, which is what let the acrophase sign error survive next to a correct copy of the same formula. `coef()` and `vcov()` take a `type` argument for either the regression coefficients or the amplitude and acrophase parameterisation.

* `confint()` returns a matrix and honours `parm`, as the `stats` generic requires, rather than a list of intervals and standard errors. An unrecognised `parm` is an error naming the parameters the model does carry, instead of a silent `NA` row. Standard errors now come from `vcov()`. This is a breaking change to the return shape, made without a deprecation window because the values it returned for the acrophase and for every population amplitude were wrong.

* `confint()` gains `method = "ellipse"`, giving the conservative limits derived from the joint confidence region for each component rather than a symmetric interval around the estimate. These respect the parameter space: an amplitude bound cannot fall below zero, and where the region covers the pole the acrophase is reported as unidentifiable rather than as an interval. `cosinor_area()` now returns those limits and a `covers_pole` flag, having previously computed them and returned only the plotting coordinates, and takes a `component` argument instead of always describing the first.

* `anova()` on a `cosinor` tests each component for a non-zero amplitude on 2 degrees of freedom, which is the question a multiple-component model raises and the package had no way to answer -- a user fitting `tau = c(24, 12)` could read off a 12-hour amplitude but not ask whether it was distinguishable from noise. Given several models it compares them sequentially. `glance()` summarises a fit in one row.

* `cosinor_order()` fits a nested family of harmonic models and reports the sequential F test and the information criteria for each, so the number of components can be chosen rather than assumed.

* `tidy()` gains `statistic` and `p.value` columns. The statistic is the per-component F test, so a component's amplitude and acrophase share it: the null being tested is that both regression coefficients are zero, and an acrophase has no meaning under it.

* `cosinor()` now refuses periods it cannot fit -- duplicated, non-positive, non-finite, or more parameters than observations -- and warns when the periods given cannot be separated by the data, reporting the design condition number. `tau = c(24, 23.5)` on `twins` returns amplitudes of 7.4 and 7.1 against a single-component amplitude of 0.30, because two near-collinear components can grow without bound so long as they cancel; this previously happened silently. See `?cosinor_identifiability` for why the condition number is used in preference to the usual spectral resolution criterion, which rejects the package's own `tau = c(24, 12)` example on folded clock time.

* `cosinor_goodness_of_fit()` and `cosinor_area()` now refuse population-mean models rather than returning a statistic with a note that it may be inaccurate. Neither quantity is defined for a pooled per-subject fit, and both would have printed a plausible number.

## Updates

* The `extract_*()` echocardiogram functions are now vectorized over `text` and return one element per report. `extract_echo_findings()` accordingly returns a tibble with one row per report instead of a list.

* `extract_la_size()` gains a `range` argument for graded descriptions such as "mildly to moderately dilated", defaulting to the upper end of the range. Grades are normalized (`"mildly"` becomes `"mild"`), and `"not well seen"` is now reported as a missing size plus a separate `la_not_visualized` flag rather than being returned as if it were a size.

* `extract_lvef()` now captures values written with an inequality, such as `<20%`, and tolerates phrasing between the term and the value ("ejection fraction by visual estimate is 55%"). `extract_echo_findings()` reports the inequality in a new `lvef_qualifier` column.

* `extract_lvidd()` no longer reads a measurement belonging to another structure. It previously took the first number to follow the term no matter how far away, so a report such as "LVIDd: not measured. LA A/P 4.3 cm" was given the left atrial diameter. It now reads only within the same clause, excludes the left atrium by name, and skips stray digits, which also makes the millimeter conversion reliable ("LVIDd 52 mm" is 5.2 cm).

* `extract_lvef()` and `extract_lvidd()` gain the `min_val` and `max_val` arguments that `extract_la_diameter()` already had, so the plausible range for each measurement can be set by the caller. `extract_lvidd()` now applies such a range at all, defaulting to 1 to 10 cm.

* Linear dimensions written without units are now resolved against that range instead of being assumed to be centimeters. Structured fields frequently omit the units, and a chamber dimension cannot be plausible in both centimeters and millimeters, so `"LVIDd: 52"` and `"LA A/P: 43"` resolve to 5.2 cm and 4.3 cm. Units the report does write are still taken at face value, so `"LVIDd 52 cm"` remains missing rather than being quietly reinterpreted.

* `extract_la_diameter()` no longer loses a measurement to its own decimal point. Its fallback keyword search split report text on `.`, which also split "4.5" in two and left neither piece holding both the keyword and the value, so that search could only ever match whole numbers. It also now recognizes measurements written in millimeters.

* Severity and diastolic dysfunction grades in `extract_echo_findings()` are now found whether the grade precedes or follows the structure ("mild mitral regurgitation" as well as "mitral regurgitation is mild"), take the upper end of graded ranges, and match valve-inclusive terms such as "mitral valve regurgitation". The ambiguous `as` and `ar` abbreviations were dropped, as they matched ordinary English.

## Next Steps

* `cosinor()` to be expanded upon to include prediction, and integration into the __tidymodels__ approach in the `parsnip` package
	* Evaluation of plotting functions for cosinor models

* Intervals reported after `cosinor_order()` has chosen the harmonic order are anticonservative, since the selection looked at the same data. This is an open problem in the cosinor literature rather than something the package currently solves.

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

