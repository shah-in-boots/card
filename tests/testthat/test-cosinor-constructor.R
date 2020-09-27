# ggplot ====
data("twins")
scos <- cosinor(rDYX ~ hour, twins, 24)
mcos <- cosinor(rDYX ~ hour, twins, c(24, 12))
pcos <- cosinor(rDYX ~ hour, twins, 24, "patid")
g <- ggcosinor(mcos)

test_that("ggcosinor makes a ggplot", {
	expect_s3_class(g, "ggplot")
})

# Parsnip ====
cosinor_mod <-
	cosinor_reg(period = c(24, 8)) %>%
	parsnip::set_engine("card") %>%
	parsnip::set_mode("regression")

cosinor_fit <-
	cosinor_mod %>%
	parsnip::fit(rDYX ~ hour, data = twins)

test_that("cosinor_reg creates appropriate parsnip model", {
	expect_that(cosinor_mod, is_a("cosinor_reg"))
	expect_s3_class(cosinor_fit$fit, "cosinor")
})

