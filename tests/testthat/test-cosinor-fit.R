# General fitting functions ----
test_that("models can be generally fit", {
	data(twins)
	scos <- cosinor(rDYX ~ hour, twins, tau = 24)
	mcos <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
	pcos <- cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")

	# Appropriate classes
	expect_s3_class(scos, "cosinor")
	expect_s3_class(mcos, "cosinor")
	expect_s3_class(pcos, "cosinor")

	# Harmonic checks
	expect_gt(length(mcos$tau), 1)
	expect_equal(max(mcos$tau) %% min(mcos$tau), 0)
	expect_warning(cosinor_features(mcos))

	# Confidence intervals
	expect_type(confint(scos), "list")
})

test_that("population cosinors can be fit", {

	# Single population cosinor
	f <- sDYX ~ hour
	data <- twins
	population = "patid"
	m <- cosinor(formula = f, data = data, tau = 24, population = population)


})
