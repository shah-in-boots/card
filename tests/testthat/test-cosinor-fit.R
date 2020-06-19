# Setup ====
data(twins)
scos <- cosinor(rDYX ~ hour, twins, tau = 24)
mcos <- cosinor(rDYX ~ hour, twins, tau = c(24, 12))
pcos <- cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")

# Cosinor objects ====

test_that("models are cosinor class", {
	expect_s3_class(scos, "cosinor")
	expect_s3_class(mcos, "cosinor")
	expect_s3_class(pcos, "cosinor")
})

# Multiple cosinor ====

test_that("cosinor is harmonic", {
	expect_gt(length(mcos$tau), 1)
	expect_equal(max(mcos$tau) %% min(mcos$tau), 0)
	expect_message(cosinor_features(mcos))

})

# Methods ====

test_that("confidence intervals work", {
	expect_type(confint(scos), "list")
})