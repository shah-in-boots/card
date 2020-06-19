# Survival tables
data("mims")

# Parameters
id <- "patid"
first <- "first_visit_date_bl"
last <- "ldka"
event.dates <- c("mi_date_1", "mi_date_2", "mi_date_3")
model.type <- "marginal"
death <- "DEATH_CV_YN"

test_that("only unique patients in original data", {
	expect_length(mims$patid, length(unique(mims$patid)))
})
