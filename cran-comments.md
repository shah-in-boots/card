## Resubmission

This is a resubmission. I'm thankful for the comments by Jelena Saf <jelena.saf@jku.at> and Swetlana Herbrandt <herbrandt@statistik.tu-dortmund.de>. In this resubmission, the following changes were made:

- The \values were added to .Rd files for all exported methods/functions.
- The options parameter in ecg_patches.R was amended with an on.exit() call to restore previously established options (to not change user working environment).

## Test environments

* MacOS 10.15, R 4.0.0
* Windows 10, R 4.0.0
* win-builder (devel andr elease)

## R CMD check results

There were no errors, warnings, or notes.

## Downstream dependencies

There are no downstream dependencies.