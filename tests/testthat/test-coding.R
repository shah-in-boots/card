test_that("code data sets can be pulled in", {

	# ICD9 and year is 2014
	dat <- procedure_codes("icd9", 2014)
	expect_s3_class(dat, "tbl_df")

	# ICD10 and year is 2023
	dat <- procedure_codes("icd10", 2023)
	expect_s3_class(dat, "tbl_df")

	# HCPCS and year is 2023
	dat <- procedure_codes("hcpcs", 2023)
	expect_s3_class(dat, "tbl_df")

	# CPT and year is 2023
	dat <- procedure_codes("cpt", 2023)
	expect_s3_class(dat, "tbl_df")

})
