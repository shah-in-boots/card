test_that("cosinor_reg creates appropriate parsnip model", {
	cosinor_mod <-
		cosinor_reg(period = c(24, 8)) %>%
		parsnip::set_engine("card") %>%
		parsnip::set_mode("regression")

	cosinor_fit <-
		cosinor_mod %>%
		parsnip::fit(rDYX ~ hour, data = twins)
	expect_that(cosinor_mod, is_a("cosinor_reg"))
	expect_s3_class(cosinor_fit$fit, "cosinor")
})

