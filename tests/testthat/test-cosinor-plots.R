test_that("ggcosinor makes a ggplot", {
  data("twins")
  scos <- cosinor(rDYX ~ hour, twins, 24)
  mcos <- cosinor(rDYX ~ hour, twins, c(24, 12))

  # The message and warning wrappers these assertions used to carry were
  # artefacts of the fitting code narrating itself on every call, not behaviour
  # worth pinning
  expect_s3_class(suppressWarnings(ggcosinor(mcos)), "ggplot")
  expect_s3_class(suppressWarnings(ggcosinor(list(scos, mcos))), "ggplot")
})
