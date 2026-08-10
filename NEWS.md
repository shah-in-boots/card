# card {development version}

## Bugs

* `get_procedure_codes()` returns codes again. It errored with `object '.cms_codes' not found` for every format and every version, in the working tree and in the released 0.1.1 alike, so no supported call worked at all. The data had been saved with `internal = FALSE`, which writes `data/.cms_codes.rda` -- a dot-prefixed file that `git add data/*.rda` silently never matches, since a shell glob skips leading dots -- and the copy that had been living in `R/sysdata.rda` was evicted when the MAUDE annex codes were later saved from their own script, taking that file from 530,220 to 63,468 bytes. `usethis::use_data(internal = TRUE)` rewrites `R/sysdata.rda` wholesale rather than adding to it, so every internal dataset now has to be named in one call, and that call lives in the new `data-raw/sysdata.R`. The object is `cms_codes` rather than `.cms_codes`; nothing referenced it by name outside the accessor.

* Rebuilding those tables turned up three parsing faults that had been bundled with them. 13 of the 1,170 CPT codes were `NA`: `vroom::vroom()` took the code column for a double from the numeric rows that lead the file, so the Category III codes, which end in `T`, failed to parse -- `0051T`, cardiac, and `0585T`, biliary, among them. The row count was unchanged either way, so the table looked complete while those procedures could not be looked up at all. 901 ICD10 codes were stored with the padding of the fixed-width field they were cut from, as `"001    "` rather than `"001"`, and matched nothing a caller would write. 79 HCPCS descriptions carried literal quote characters, reading `"\"Fluzone vacc, 3 yrs & >, im\""`, the quotes having been stripped from the code alone. The generator now checks its own output for missing, empty and untrimmed codes before saving, since a parser that drops codes leaves the row count untouched.

* `get_procedure_codes()` accepts `format` case-insensitively, as it has always been documented to. The check was a bare `%in%` against lowercase names, so `get_procedure_codes("ICD9", 2014)` was refused as an unsupported format.

* `maude_query()` now returns the device problem terms it promises. The `device_problem` column was read from `device[].device_problem_codes`, which the openFDA device event endpoint never returns; the terms are at the top level of the record, in `product_problems`. Nothing errored and nothing warned, so the column arrived present, typed, and `NA` for every row of every query ever run, which reads as "MAUDE does not code device problems for these reports" rather than as a bug. In a 41,403-report extract of pulsed-field ablation reports it was empty throughout and the malfunction-mechanism analysis was nearly abandoned on that basis. Note the asymmetry that caused it: `device.device_problem_codes` is a valid field to *search* on, so the `device_problem` argument was always filtering correctly on a field that is never returned.

* `load_maude_codes()` no longer gives 26 of the 27 Annex A families the wrong term. The bundled table was built by filling the level columns downward, which carries the previous family's last child into the next family row -- a family's Level 2 cell is blank because a family has no Level 2 term, not because the value is missing. `A02` was therefore returned as `"Implant subsidence"` rather than `"Manufacturing, Packaging or Shipping Problem"`. FDA codes a large share of reports at the family level, so joining returned MAUDE terms to this table on `term` silently dropped them: measured on the same pulsed-field extract, leaf-only matching left 3,466 of 11,683 device problem mentions unmatched. The hierarchy is now read from the IMDRF code, which spells the ancestry out exactly, and `level_1` is populated for every row of every annex. It was previously missing on 783 of 807 Annex E rows and 43 of 72 Annex F rows, so those two annexes had effectively no hierarchy at all.

* `maude_query()` no longer repeats each coded term. openFDA emits the `product_problems` and `patient.patient_problems` arrays twice for most records, an artefact of how the endpoint joins its tables, so a single-device report came back as `"Pericarditis; Pericarditis"`. Narrative blocks are still returned in full: each carries its own `mdr_text_key` and a supplement may legitimately repeat the text of the report it amends.

* `cosinor_reg()` can be fitted through __parsnip__ again. The engine registration never called `parsnip::set_encoding()`, and since `parsnip::get_encoding()` returns `NULL` for a missing table rather than raising an error, the defaults it would otherwise have supplied never applied and the fit failed inside __vctrs__ with a message naming neither the model nor the engine. A specification still built and printed correctly, so nothing was visibly wrong until something was fitted with it. The registration is also no longer skipped wholesale once the model exists -- only `set_new_model()` is guarded now, the remaining setters being idempotent -- so an incomplete registration repairs itself on the next load instead of persisting for the life of the session. The documented example now fits rather than stopping at the specification, and the test for it is no longer skipped.

* `confint()` on a population-mean cosinor no longer reports amplitude and acrophase intervals that are too wide by a factor of the square root of the number of subjects. The mesor variance was divided by the subject count while the amplitude and acrophase variances were not, so the 95% interval for the amplitude of the 24-hour rhythm in `twins` came back as -0.399 to 0.975 -- an interval covering zero, and impossible values below it, for a rhythm measured across 741 subjects. It is 0.263 to 0.313.

* The acrophase standard error carried the wrong sign on its cross term. The delta method gives the amplitude a negative cross term and the acrophase a positive one, and both were written negative. The error is invisible whenever time is sampled evenly across whole cycles, since it acts only through the covariance of the two regression coefficients, which is why the bundled data never showed it; on an unevenly sampled design it inflated the standard error by around 37%.

* `cosinor_zero_amplitude()` now scales with the number of components. It fixed the numerator at 2 degrees of freedom and the denominator at `N - 3` whatever `tau` was given, so a two-component model on `twins` reported `F = 707` on 2 and 16383 degrees of freedom where the test is `F = 354` on 4 and 16381. It also returns a p-value.

* Fitted values and residuals from a population-mean cosinor are now aligned with the rows they came from. They were returned in subject order while the outcome stayed in input order, so both were correct only when the data happened to arrive sorted by subject.

* `cosinor()` now accepts non-numeric subject identifiers in `population`. The insufficient-observations filter coerced the subject names with `as.numeric()`, which made every one of them `NA` for character identifiers, so no subject was dropped and the per-subject fit then failed on a singular matrix.

* `cosinor_features()` no longer scrambles the components of a multiple-component population model when reconstructing its fitted curve, and its harmonic check now considers every period rather than only the longest and shortest, so `tau = c(24, 5, 12)` is correctly reported as non-harmonic.

## Features

* `normalize_maude_manufacturer()` and the bundled `maude_manufacturer_index` map MAUDE's manufacturing entities onto a canonical name. `manufacturer_name` is the plant, not the company, so counting the raw strings splits Biosense Webster across four plants plus `STOCKERT GMBH`, divides Boston Scientific's pulsed-field reports between `BOSTON SCIENTIFIC CORPORATION` and `FARAPULSE, INC.`, and hides Abbott behind `VENUSA DE MEXICO S.A. DE C.V.`, a contract manufacturer. A string the index does not cover returns `NA` rather than a guess.

* `resolve_maude_owner()` and the bundled `maude_ownership` answer the separate question of who owned an entity, and when. Ownership is a dated parent-pointer table rather than a company name baked into each pattern, which is what makes an acquisition **one row**: `"Telectronics Pacing Systems"` reaches Abbott through St. Jude Medical without anyone writing Abbott next to Telectronics, and acquiring a parent carries every entity beneath it. It also expresses a divestiture, which a single-parent column cannot -- an entity with no row matching the date owns itself, so Physio-Control resolves to Medtronic in 2005, to itself in 2014, and to Stryker in 2020. `as_of` has deliberately no default: pass `Sys.Date()` for the company that owns the entity now, or a report's `date_received` for the company that owned it when the report was filed. A `Sys.Date()` default would have made the same code return different answers after a future acquisition without anything in the call having changed.

* `normalize_maude_ablation()` and the bundled `maude_ablation_index` derive the ablation platform, energy modality and maker from the brand name. Product code does not determine modality -- `OAE` covers cryoablation and radiofrequency alike -- so a rule resolving it from the product code and falling back to radiofrequency puts cryoballoon, pulsed-field and laser devices in a radiofrequency arm. Two things it is careful about: `"arctic"` as a substring is not a cryoablation signal, since `ARCTIC SUN` is a temperature management console, and the mapping, access and irrigation devices that share the ablation product codes (`PENTARAY`, `OCTARAY`, `FARADRIVE`, `RHYTHMIA`, `ENSITE`) are matched but carry no modality rather than being swept into an arm. The table names the entity that makes each platform, never its corporate parent, so it does not have to be edited when a company is acquired.

* All three tables are curated as CSVs under `data-raw/maude-entities/`, one fact per row, so that a diff shows a single changed line and so that they can be edited without writing R. Precedence is an explicit `priority` column rather than row position -- `CRYOCATH` is tested before `MEDTRONIC` because reports arrive as `"MEDTRONIC CRYOCATH LP"` -- which means the files can be sorted for review without changing behaviour. Measured coverage travels with the data as a `coverage` attribute rather than as a number in the documentation that would go stale, and the full report, including patterns that matched nothing and the largest strings still falling through, is regenerated into `data-raw/maude-entities/coverage.md` on every rebuild.

* `maude_fda_api_call()` gains a `count` argument, reaching the openFDA `count` endpoint. It aggregates on the server and returns a two-column tibble of `term` and `count`, so a device problem frequency table over 41,000 reports takes one request rather than roughly 414 paginated ones. Two things it will not tell you, both documented in `?maude_query`: the endpoint caps at 1000 terms with no pagination cursor, and the counts are of mentions rather than of reports. `limit`, `skip` and `api_key` now have defaults, so a count call needs only the query and the field.

* `maude_query()` warns when the query matched more reports than `limit` returned, and carries the matched count on the result as a `"total"` attribute. `limit` is a cap on the call, and a query matching 41,000 reports and a query matching exactly `limit` of them previously came back looking identical.

* `maude_adjudicate()` records what produced an adjudication, as `"adjudication_model"`, `"prompt_hash"` and `"definitions_hash"` attributes on the returned list. The internal system prompt and `complication_definitions` will both change, and a dataset adjudicated under one pair is not comparable to a dataset adjudicated under another, but nothing in the flags themselves said which was used.

* `cosinor()` objects gain the standard extractor methods -- `coef()`, `vcov()`, `sigma()`, `nobs()`, `df.residual()` and `logLik()`, and through the last of these `AIC()` and `BIC()`. Every statistic the package reports is now derived from `vcov()` rather than rebuilding the covariance matrix in each function, which is what let the acrophase sign error survive next to a correct copy of the same formula. `coef()` and `vcov()` take a `type` argument for either the regression coefficients or the amplitude and acrophase parameterisation.

* `confint()` returns a matrix and honours `parm`, as the `stats` generic requires, rather than a list of intervals and standard errors. An unrecognised `parm` is an error naming the parameters the model does carry, instead of a silent `NA` row. Standard errors now come from `vcov()`. This is a breaking change to the return shape, made without a deprecation window because the values it returned for the acrophase and for every population amplitude were wrong.

* `confint()` gains `method = "ellipse"`, giving the conservative limits derived from the joint confidence region for each component rather than a symmetric interval around the estimate. These respect the parameter space: an amplitude bound cannot fall below zero, and where the region covers the pole the acrophase is reported as unidentifiable rather than as an interval. `cosinor_area()` now returns those limits and a `covers_pole` flag, having previously computed them and returned only the plotting coordinates, and takes a `component` argument instead of always describing the first.

* `anova()` on a `cosinor` tests each component for a non-zero amplitude on 2 degrees of freedom, which is the question a multiple-component model raises and the package had no way to answer -- a user fitting `tau = c(24, 12)` could read off a 12-hour amplitude but not ask whether it was distinguishable from noise. Given several models it compares them sequentially. `glance()` summarises a fit in one row.

* `cosinor_order()` fits a nested family of harmonic models and reports the sequential F test and the information criteria for each, so the number of components can be chosen rather than assumed.

* `tidy()` gains `statistic` and `p.value` columns. The statistic is the per-component F test, so a component's amplitude and acrophase share it: the null being tested is that both regression coefficients are zero, and an acrophase has no meaning under it.

* `cosinor()` now refuses periods it cannot fit -- duplicated, non-positive, non-finite, or more parameters than observations -- and warns when the periods given cannot be separated by the data, reporting the design condition number. `tau = c(24, 23.5)` on `twins` returns amplitudes of 7.4 and 7.1 against a single-component amplitude of 0.30, because two near-collinear components can grow without bound so long as they cancel; this previously happened silently. See `?cosinor_identifiability` for why the condition number is used in preference to the usual spectral resolution criterion, which rejects the package's own `tau = c(24, 12)` example on folded clock time.

* `cosinor_goodness_of_fit()` and `cosinor_area()` now refuse population-mean models rather than returning a statistic with a note that it may be inaccurate. Neither quantity is defined for a pooled per-subject fit, and both would have printed a plausible number.

## Updates

* `maude_fda_api_call()` explains an HTTP 403 rather than passing openFDA's message through. openFDA answers any refused anonymous request with "No api_key was supplied", which is what it says whether the per-minute rate was exceeded or `limit` was set above 999 -- neither of which needs a key to fix.

* `load_maude_codes()` documents the annex hierarchy and the polyhierarchy in Annex E, where a term such as "Brain Injury" belongs to two families and so appears on one row per parent. That is correct, and it will double-count if a join on `term` is then tallied without reducing to `imdrf_code` first.

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

* `procedure_codes()` is renamed to `get_procedure_codes()`, following the package's own `get_*()` convention for accessors. The old name reads as a dataset and tab-completes next to `complication_definitions` and `maude_complication_index`, which are data, while it is in fact a function of `format` and `version`. The old name still works and warns once per session rather than once per call, since these are looked up in a loop over codes.

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

