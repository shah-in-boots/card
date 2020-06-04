# Cosinor arguments
test_that("outputs/arguments are appropriate for cosinor", {

	# Setup
	data("twins")
	m <- cosinor(rDYX ~ hour, twins, tau = 24)

	# Tests
	expect_s3_class(m, "cosinor")
  expect_vector(m$tau)

})
