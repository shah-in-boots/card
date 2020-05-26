## Resubmission

This is a resubmission. I'm thankful for the comments by Jelena Saf <jelena.saf@jku.at>. With this, I have made the following corrections:

- Explained abbreviations/acronyms in DESCRIPTION 
- Added copyright holder to authors
- Add citations of theoretical literature supporting package methods in DESCRIPTION under "description" field with DOI linkage
- Examples were fixed such that code was not commented out 
- Vignettes were updated to reflect full functionality of software (instead of being "works-in-progress")
- All functions were reviewed to make sure no changes were made to user options, including no writing to the user directory by `getwd()`, or user options. 
- If this is found to be incorrect, please let me know and I will learn to improve the package / functions.

## Test environments

* MacOS 10.15, R 4.0.0
* Windows 10, R 4.0.0
* win-builder (devel andr elease)

## R CMD check results

There were no errors, warnings, or notes.

## Downstream dependencies

There are no downstream dependencies.