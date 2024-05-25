# AF Composite Score testing ----

test_that("afib_score calculates correctly with AFEQT data", {
	set_afib_score_parameters(
		weight_afeqt_total = 0.4,
		weight_afeqt_symptoms = 0.2,
		weight_afeqt_activities = 0.2,
		weight_afeqt_treatment = 0.2,
		weight_nyha_class = 1,
		weight_electrical = 0.5,
		mct_cutoff_low = 0.33,
		mct_cutoff_medium = 0.66,
		ecg_cutoff_low = 0.33,
		ecg_cutoff_medium = 0.66
	)

	parameters <- getOption("afib_parameters")

	result <- afib_score(
		symptom_type = "AFEQT",
		afeqt_total = 80,
		afeqt_symptoms = 70,
		afeqt_activities = 85,
		afeqt_treatment = 75,
		electrical_type = "MCT",
		mct_burden = 0.2
	)

	expected_symptom_burden <- 80 * parameters$weight_afeqt_total +
		70 * parameters$weight_afeqt_symptoms +
		85 * parameters$weight_afeqt_activities +
		75 * parameters$weight_afeqt_treatment

	expected_electrical_burden <- 1 * parameters$weight_electrical

	expected_result <- expected_symptom_burden * parameters$weight_symptoms + 1 * parameters$weight_electrical

	expect_equal(result, expected_result)
})

test_that("afib_score calculates correctly with NYHA data", {
	set_afib_score_parameters(
		weight_afeqt_total = 0.4,
		weight_afeqt_symptoms = 0.2,
		weight_afeqt_activities = 0.2,
		weight_afeqt_treatment = 0.2,
		weight_nyha_class = 1,
		weight_electrical = 0.5,
		mct_cutoff_low = 0.33,
		mct_cutoff_medium = 0.66,
		ecg_cutoff_low = 0.33,
		ecg_cutoff_medium = 0.66
	)

	parameters <- getOption("afib_parameters")

	result <- afib_score(
		symptom_type = "NYHA",
		nyha_class = "III",
		electrical_type = "ECG",
		ecg_total = 10,
		ecg_sinus = 7,
		ecg_time_range = "12 months"
	)

	nyha_scale <- c("I" = 25, "II" = 50, "III" = 75, "IV" = 100)
	expected_symptom_burden <- nyha_scale["III"] * parameters$weight_nyha_class

	expected_electrical_burden <- 2 * parameters$weight_electrical

	expected_result <- expected_symptom_burden * parameters$weight_symptoms + expected_electrical_burden

	expect_equal(result, expected_result)
})

test_that("afib_score errors with invalid symptom type", {
	expect_error(afib_score(symptom_type = "INVALID"))
})

test_that("afib_score errors with invalid electrical type", {
	expect_error(afib_score(
		symptom_type = "AFEQT",
		afeqt_total = 80,
		afeqt_symptoms = 70,
		afeqt_activities = 85,
		afeqt_treatment = 75,
		electrical_type = "INVALID"
	))
})

test_that("afib_score errors with missing MCT data", {
	expect_error(afib_score(
		symptom_type = "AFEQT",
		afeqt_total = 80,
		afeqt_symptoms = 70,
		afeqt_activities = 85,
		afeqt_treatment = 75,
		electrical_type = "MCT"
	))
})

test_that("afib_score errors with missing ECG data", {
	expect_error(afib_score(
		symptom_type = "NYHA",
		nyha_class = "III",
		electrical_type = "ECG",
		ecg_total = 10,
		ecg_time_range = "12 months"
	))
})


# AF score weights and options ----

test_that("set_afib_score_parameters updates global options", {
  set_afib_score_parameters(weight_afeqt_total = 0.5)
  params <- getOption("afib_parameters")
  expect_equal(params$weight_afeqt_total, 0.5)

  set_afib_score_parameters(weight_afeqt_symptoms = 0.3)
  params <- getOption("afib_parameters")
  expect_equal(params$weight_afeqt_symptoms, 0.3)

  set_afib_score_parameters(weight_afeqt_activities = 0.25)
  params <- getOption("afib_parameters")
  expect_equal(params$weight_afeqt_activities, 0.25)

  set_afib_score_parameters(weight_afeqt_treatment = 0.15)
  params <- getOption("afib_parameters")
  expect_equal(params$weight_afeqt_treatment, 0.15)

  set_afib_score_parameters(weight_nyha_class = 0.8)
  params <- getOption("afib_parameters")
  expect_equal(params$weight_nyha_class, 0.8)

  set_afib_score_parameters(weight_electrical = 0.6)
  params <- getOption("afib_parameters")
  expect_equal(params$weight_electrical, 0.6)

  set_afib_score_parameters(mct_cutoff_low = 0.2)
  params <- getOption("afib_parameters")
  expect_equal(params$mct_cutoff_low, 0.2)

  set_afib_score_parameters(mct_cutoff_medium = 0.5)
  params <- getOption("afib_parameters")
  expect_equal(params$mct_cutoff_medium, 0.5)

  set_afib_score_parameters(ecg_cutoff_low = 0.2)
  params <- getOption("afib_parameters")
  expect_equal(params$ecg_cutoff_low, 0.2)

  set_afib_score_parameters(ecg_cutoff_medium = 0.5)
  params <- getOption("afib_parameters")
  expect_equal(params$ecg_cutoff_medium, 0.5)
})

