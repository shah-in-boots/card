test_that("icd9 codes can be extracted", {
	dat <- procedure_codes("icd9", "2014")
	expect_s3_class(dat, "data.frame")
})
