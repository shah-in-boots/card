# Tracks which superseded functions have already warned, so a rename is
# reported once per session rather than once per call. A deprecation announced
# 14,000 times inside a loop is noise that hides everything around it.
.deprecated <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Loads cosinor_reg in the model database
  make_cosinor_reg()
}
