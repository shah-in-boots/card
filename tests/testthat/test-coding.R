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

	# Accept character options as well for year
	dat <- procedure_codes("icd9", "2014")
	expect_s3_class(dat, "tbl_df")

	# But do not accept characters that do not convert to years
	expect_error(procedure_codes("icd9", "2014-10-01"))

	# THe format must also be acceptable
	expect_error(procedure_codes("icd11", 2014))

	# The format and version argument can only accept one scalar each
	expect_error(procedure_codes("icd9", c(2014, 2015)))
	expect_error(procedure_codes("icd9", c(2014, 2015), "2023"))
	expect_error(procedure_codes(c("icd9", "icd10"), 2014))

	# The version must be appropriate for the format
	expect_error(procedure_codes("hcpcs", 2014))
	expect_error(procedure_codes("icd10", 2014))
	expect_error(procedure_codes("cpt", 2014))

})
