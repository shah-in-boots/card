# Cosinor objects are of S3 class
test_that("cosinor objects are of class cosinor", {

	data("twins")
	m <- cosinor(rDYX ~ hour, twins, tau = 24)
	expect_s3_class(m, "cosinor")

})
