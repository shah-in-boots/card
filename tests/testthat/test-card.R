context("circ_sun")

test_that("circ_sun should have enough arguments", {
	expect_error( circ_sun("05/01/2020", -42.0) )
})
