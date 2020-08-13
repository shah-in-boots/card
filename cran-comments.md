## Resubmission

This is a resubmission. I'm thankful for the comments by Jelena Saf <jelena.saf@jku.at>, Swetlana Herbrandt <herbrandt@statistik.tu-dortmund.de>, and Martina Schmirl <martina.schmirl@jku.at>. In this resubmission, the following changes were made:

- The functions that required an options parameter were removed and/or the functions were amended with an on.exit() call to restore previously established options (to not change user working environment)
- The print-messages were removed from non-base print/summary style functions, and instead message/warnings were used to communicate with the console in a more appropriate manner for R.

## Test environments

* MacOS 10.15, R 4.0.0
* Windows 10, R 4.0.0
* win-builder (devel and release)

## R CMD check results

There were no errors, warnings, or notes.

## Downstream dependencies

There are no downstream dependencies.