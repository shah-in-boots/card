test_that("summary function has cosinor objects", {
	# Setup
	data(twins)
	object <- cosinor(rDYX ~ hour, twins, 24)

	# Appropriate input
	expect_s3_class(object, "cosinor")

	# Appropriate output includes call
	expect_output(summary(object), "Call")
})
