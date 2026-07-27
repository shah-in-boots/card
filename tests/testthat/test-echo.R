test_that("LA size takes the requested end of a graded range", {
  # Graded ranges are common and should not silently return the lower bound
  txt <- "the left atrium is mildly to moderately dilated"
  expect_equal(extract_la_size(txt), "moderate")
  expect_equal(extract_la_size(txt, range = "lower"), "mild")

  expect_equal(
    extract_la_size("left atrium: moderate to severely dilated"),
    "severe"
  )

  # Single grades are unaffected by the range argument
  expect_equal(extract_la_size("the left atrium is mildly dilated"), "mild")

  # Surface forms are normalized so the result can be an ordered factor
  expect_equal(extract_la_size("left atrial size is very severely dilated"), "severe")
  expect_equal(extract_la_size("the left atrium is likely normal"), "normal")

  expect_true(is.na(extract_la_size("no chamber comment")))
})

test_that("LA size separates measurability from size", {
  # A definite grade later in the report should win over an indeterminate one
  both <- paste(
    "left atrium: the left atrium is not well seen.",
    "the left atrium is severely dilated."
  )
  expect_equal(extract_la_size(both), "severe")
  expect_true(extract_echo_findings(both)$la_not_visualized)

  # Without a definite grade the size is missing, not "not well seen"
  only <- "left atrium: the left atrium is not well seen."
  expect_true(is.na(extract_la_size(only)))
  expect_true(extract_echo_findings(only)$la_not_visualized)

  expect_false(extract_echo_findings("the left atrium is mildly dilated")$la_not_visualized)
})

test_that("LVEF captures inequality qualifiers and intervening phrasing", {
  low <- "overall left ventricular ejection fraction by visual estimate is <20%"
  expect_equal(extract_lvef(low), 20)
  expect_equal(extract_echo_findings(low)$lvef_qualifier, "<")
  expect_equal(extract_echo_findings(low)$lvef_category, "severe systolic dysfunction")

  expect_equal(extract_lvef("left ventricular ejection fraction >65%"), 65)

  # Values reported without a qualifier carry NA rather than an empty string
  expect_true(is.na(extract_echo_findings("lvef: 55%")$lvef_qualifier))

  # Stray digits should not be mistaken for the value
  expect_equal(extract_lvef("lvef by 2d simpson is 55%"), 55)

  # Ranges still resolve to the lower bound
  expect_equal(extract_lvef("ef 55-60%"), 55)

  # Implausible values are dropped
  expect_true(is.na(extract_lvef("ef 200%")))
})

test_that("severity is found whether the grade leads or follows the term", {
  findings <- extract_echo_findings("mitral valve: trivial mitral valve regurgitation")
  expect_equal(findings$mitral_regurgitation, "trivial")

  expect_equal(
    extract_echo_findings("mitral regurgitation is severe")$mitral_regurgitation,
    "severe"
  )

  # Negation normalizes to a single level
  expect_equal(
    extract_echo_findings("pericardium: there is no pericardial effusion")$pericardial_effusion,
    "none"
  )

  # Graded ranges resolve to the upper end
  expect_equal(
    extract_echo_findings("mild to moderate mitral regurgitation")$mitral_regurgitation,
    "moderate"
  )

  # Neighboring structures keep their own grade
  listed <- extract_echo_findings(
    "there is mild mitral regurgitation and moderate tricuspid regurgitation"
  )
  expect_equal(listed$mitral_regurgitation, "mild")
  expect_equal(listed$tricuspid_regurgitation, "moderate")

  # The word "as" must not be read as aortic stenosis
  expect_true(is.na(
    extract_echo_findings("severe symptoms as compared with the prior study")$aortic_stenosis
  ))
})

test_that("diastolic dysfunction grade is found in either direction", {
  expect_equal(
    extract_echo_findings("grade ii diastolic dysfunction")$lv_diastolic_dysfunction,
    "grade ii"
  )
  expect_equal(
    extract_echo_findings("diastolic function: normal")$lv_diastolic_dysfunction,
    "normal"
  )
})

test_that("LVIDd reads only the ventricle's own measurement", {
  expect_equal(extract_lvidd("lvidd 5.2 cm"), 5.2)
  expect_equal(extract_lvidd("lvid(d): 4.9 cm"), 4.9)
  expect_equal(extract_lvidd("lv end diastolic dimension (2d, plax) 5.4 cm"), 5.4)

  # Millimeters are converted, and the units must not be split off the value
  expect_equal(extract_lvidd("lvidd 52 mm"), 5.2)

  # Phrasing between the term and the value is tolerated
  expect_equal(
    extract_lvidd("lv end diastolic dimension measured from the parasternal long axis is 5.2 cm"),
    5.2
  )
  expect_equal(extract_lvidd("lvidd, indexed to body surface area, 5.1 cm"), 5.1)

  # A measurement in a later clause belongs to another structure
  expect_true(is.na(extract_lvidd("lvidd: not measured. la a/p 4.3 cm")))
  expect_true(is.na(extract_lvidd("lvidd normal; la a/p 4.3 cm")))
  expect_equal(
    extract_lvidd("left ventricle: normal. lvidd 5.1 cm. la a/p: 4.3 cm"),
    5.1
  )

  # The atrium is excluded by name, since an LA diameter is a plausible LVIDd
  # and the two would silently become the same number
  expect_true(is.na(extract_lvidd("lvidd normal with la a/p 4.3 cm")))
  expect_true(is.na(extract_lvidd("lvidd not measured, la a/p 4.3 cm")))
  expect_equal(extract_lvidd("lvidd: 5.2 cm la a/p: 4.3 cm"), 5.2)

  # The ventricle and the atrium stay independent across a report
  findings <- extract_echo_findings("lvidd: 5.2 cm la a/p: 4.3 cm")
  expect_equal(findings$lvidd_cm, 5.2)
  expect_equal(findings$la_diameter_cm, 4.3)

  # Stray digits should not be mistaken for the value
  expect_equal(extract_lvidd("lv edd by 2d is 4.8 cm"), 4.8)

  # Implausible values are dropped, including millimeters written without units
  expect_true(is.na(extract_lvidd("lvidd 52")))
  expect_true(is.na(extract_lvidd("lvidd 0.2 cm")))
})

test_that("plausible ranges are exposed on the numeric extractors", {
  expect_true(is.na(extract_lvef("lvef 55%", max_val = 50)))
  expect_equal(extract_lvef("ef 95%", max_val = 99), 95)

  expect_true(is.na(extract_lvidd("lvidd 5.2 cm", max_val = 4)))
  expect_equal(extract_lvidd("lvidd 52", max_val = 100), 52)

  expect_true(is.na(extract_la_diameter("la a/p: 4.3 cm", max_val = 4)))
})

test_that("extractors are vectorized over reports", {
  reports <- c(
    "the left atrium is mildly dilated. lvef: 55%. la a/p: 4.3 cm. lvidd 5.2 cm.",
    "the left atrium is severely dilated. lvef 25%.",
    NA
  )

  expect_equal(extract_la_size(reports), c("mild", "severe", NA))
  expect_equal(extract_lvef(reports), c(55, 25, NA))
  expect_equal(extract_la_diameter(reports), c(4.3, NA, NA))
  expect_equal(extract_lvidd(reports), c(5.2, NA, NA))

  findings <- extract_echo_findings(reports)
  expect_s3_class(findings, "tbl_df")
  expect_equal(nrow(findings), 3)

  # Empty input gives an empty result rather than an error
  expect_length(extract_la_size(character(0)), 0)
  expect_equal(nrow(extract_echo_findings(character(0))), 0)
})
