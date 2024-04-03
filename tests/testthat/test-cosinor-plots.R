test_that("ggcosinor makes a ggplot", {
	data("twins")
	scos <- cosinor(rDYX ~ hour, twins, 24)
	mcos <- cosinor(rDYX ~ hour, twins, c(24, 12))
	pcos <- cosinor(rDYX ~ hour, twins, 24, "patid")
	g <- ggcosinor(mcos)
	expect_s3_class(g, "ggplot")
})