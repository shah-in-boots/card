# card (development version)

## In Development

* Working on confidence interval extraction and prediction functions for `cosinor()`.

* Creating a function to center time series data around an event.

* Improve functionality of ECG patch intake and cleaning. A lot of these functions are to extend or work with the [MATLAB HRV Toolbox](https://alpha.physionet.org/content/pcst/1.0.0/Tools/ECG_Analysis_Tools/). 

* Adding first vignette on using `cosinor()` and `ggcosinorfit()` in the analysis of heart rate variability data over time.

* Add vignette on correcting for circadian rhythm in time series analysis, using the `circ_sun()` and `circ_center()` functions.

* Add vignette on using `recur_survival_table()` by expanding examples.

## Features

* Circadian rhythm analysis has also created an initial family of functions that will work to simplify the process of analyzing 24-hour data. The `circ_compare_groups()` helps to summarize circadian data by an covariate and time. This is visualized using `ggcircadian()`. Also includes the `ggforest()` to create forest plots of odds ratios. This is dependent on the `circ_odds()` function to generate odds ratios by time.

* An important regression function, built with the _tidymodels_ [hardhat](https://tidymodels.github.io/hardhat/) package, `cosinor()` introduced as a new function to allow for diagnostic analysis of circadian patterns. Although the algorithm is well known, having an implementation in R allows potential diagnostics. This includes the `ggcosinorfit()` allows for assessing rhythmicity and confidence intervals of amplitude and acrophase of cosinor model.

* Recurrent events can now be analyzed using a powerful function called `recur_survival_table()`, which allows for redesigning longitudinal data tables into a model appropriate for analysis. It is built to extend survival analyses. The `recur_summary_table()` function allows for reviewing the findings from recurrent events by category to help understand event strata.

* The `circ_sun()` function allows for identifying the sunrise and sunset times based on geographical location. This is intended to couple with the `circ_center()` function to center a time series around an event, such as sunrise.

## Bugs

* A number of functions are internal facing, and have not yet been checked for compatibility or generalizability. Particularly, there will need to be an overhaul of the package structure with regards to heart rate variability modeling.

* There are issues with the dataset validity, which makes the examples not run as expected. The functions currently exported work, but the examples are not yet the most enlightening.