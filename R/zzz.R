# Tracks which superseded functions have already warned, so a rename is
# reported once per session rather than once per call. A deprecation announced
# 14,000 times inside a loop is noise that hides everything around it.
.deprecated <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Loads cosinor_reg in the model database
  make_cosinor_reg()
}

# Column names used unquoted inside dplyr verbs; R CMD check cannot see the
# data frame they belong to.
utils::globalVariables(c(
  "gene_symbol", "clinical_significance", "phenotypes", "chromosome",
  "n_pathogenic", "n_variants"
))
