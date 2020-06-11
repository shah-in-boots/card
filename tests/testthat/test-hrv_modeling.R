# Model building
test_that("sequential model building uses a formula", {
	data(geh)
	f <- svg_mag + qrs_tang ~ lab_hba1c + bmi
	expect_s3_class(f, "formula")
})