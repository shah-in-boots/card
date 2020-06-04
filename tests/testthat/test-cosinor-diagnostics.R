# ggcosinor
test_that("cosinor objects are appropriate for plotting", {
	# Data
	data("twins")
	sad <- cosinor(rDYX ~ hour, twins[twins$sad_bin == 1,], tau = 24)
	happy <- cosinor(rDYX ~ hour, twins[twins$sad_bin == 0, ], tau = 24)
	pop <- cosinor(rDYX ~ hour, twins, tau = 24, population = "patid")
	objects <- list(sad, happy)

	# Cosinor class
	expect_s3_class(sad, "cosinor")
	expect_type(objects, "list")

	# Ggplot types
	gg <- ggcosinor(sad)
	expect_s3_class(gg, "ggplot")

	# Ggplot for multiples...
	ggs <- ggmulticosinor(objects)
	expect_vector(objects)

	# Population type
	expect_match(sad$type, "Individual")
	expect_match(pop$type, "Population")

})

