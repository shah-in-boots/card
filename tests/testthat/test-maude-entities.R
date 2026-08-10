# Invariants of the bundled MAUDE entity tables.
#
# These duplicate the checks in `data-raw/maude-entities.R` on purpose. That
# script runs only when a human sources it with network access, so without these
# a bad edit to one of the CSVs would ship. Everything here is offline.

# Real MAUDE strings with the value each should resolve to, taken from the
# openFDA `count` endpoint and hand-labelled. It is what makes the shadowing
# check below mean something without a network call.
maude_strings <- function() {
  utils::read.csv(
    testthat::test_path("sample-maude-strings.csv"),
    stringsAsFactors = FALSE,
    na.strings = "NA"
  )
}

test_that("no pattern is duplicated within an index", {
  expect_false(anyDuplicated(maude_manufacturer_index$pattern) > 0)
  expect_false(anyDuplicated(maude_ablation_index$pattern) > 0)
})

test_that("every ablation row carries a platform, an entity, and a known modality", {
  expect_false(anyNA(maude_ablation_index$platform))
  expect_false(anyNA(maude_ablation_index$entity))
  expect_true(all(
    maude_ablation_index$modality %in% c(
      "cryoablation", "radiofrequency", "pulsed field",
      "pulsed field or radiofrequency", "laser", NA_character_
    )
  ))
})

test_that("every entity named by an index is known to the ownership table", {
  # An index naming an entity the ownership table has never heard of is usually
  # a typo, and it resolves to itself silently rather than erroring.
  referenced <- unique(c(
    maude_manufacturer_index$entity,
    maude_ablation_index$entity
  ))
  known <- unique(c(maude_ownership$entity, maude_ownership$parent))

  # A terminal company -- Biotronik, Terumo -- correctly has no ownership row,
  # so what is checked is that every entity is either known to the ownership
  # table or resolves to itself unchanged.
  terminal <- setdiff(referenced, known)
  expect_identical(
    resolve_maude_owner(terminal, as_of = Sys.Date()),
    terminal
  )
})

test_that("ownership intervals are well formed and do not overlap", {
  ok <- is.na(maude_ownership$from) | is.na(maude_ownership$to) |
    maude_ownership$from < maude_ownership$to
  expect_true(all(ok))

  # Two owners at once would make the resolver pick one silently.
  for (rows in split(maude_ownership, maude_ownership$entity)) {
    if (nrow(rows) < 2L) {
      next
    }
    starts <- dplyr::coalesce(rows$from, as.Date("1900-01-01"))
    ends <- dplyr::coalesce(rows$to, as.Date("2999-12-31"))
    overlaps <- outer(starts, ends, `<`) & outer(ends, starts, `>`) &
      !diag(TRUE, nrow(rows))
    expect_false(any(overlaps), label = rows$entity[[1]])
  }
})

test_that("the ownership graph resolves for every entity it knows", {
  # `resolve_maude_owner()` errors rather than looping if the table ever
  # contains a cycle, so this trips on one.
  known <- unique(c(maude_ownership$entity, maude_ownership$parent))
  expect_no_error(resolve_maude_owner(known, as_of = as.Date("1992-01-01")))
  expect_no_error(resolve_maude_owner(known, as_of = Sys.Date()))
})

test_that("resolve_maude_owner follows a chain rather than a single hop", {
  # The reason ownership is its own table. Each of these was previously a
  # hardcoded parent with the chain written out in a prose note, so acquiring
  # Abbott meant re-deriving six rows by hand.
  expect_identical(
    resolve_maude_owner(
      "Telectronics Pacing Systems",
      as_of = as.Date(c("1995-01-01", "2000-01-01", "2020-01-01"))
    ),
    c("Telectronics Pacing Systems", "St. Jude Medical", "Abbott")
  )

  # Three hops: Galil Medical to BTG to Boston Scientific.
  expect_identical(
    resolve_maude_owner(
      "Galil Medical",
      as_of = as.Date(c("2015-01-01", "2018-01-01", "2024-01-01"))
    ),
    c("Galil Medical", "BTG", "Boston Scientific")
  )

  expect_identical(
    resolve_maude_owner(
      "ev3",
      as_of = as.Date(c("2009-01-01", "2012-01-01", "2020-01-01"))
    ),
    c("ev3", "Covidien", "Medtronic")
  )
})

test_that("resolve_maude_owner expresses a divestiture", {
  # An entity with no row matching the date owns itself, which is the only way a
  # parent-pointer table can say "this was sold". A single parent column cannot:
  # Physio-Control was Medtronic's until 2012 and Stryker's from 2016, and was
  # neither in between.
  expect_identical(
    resolve_maude_owner(
      "Physio-Control",
      as_of = as.Date(c("2005-01-01", "2014-01-01", "2020-01-01"))
    ),
    c("Medtronic", "Physio-Control", "Stryker")
  )

  expect_identical(
    resolve_maude_owner(
      "Cordis",
      as_of = as.Date(c("2019-01-01", "2023-01-01"))
    ),
    c("Johnson & Johnson", "Cordis")
  )
})

test_that("resolve_maude_owner refuses to guess a date", {
  expect_error(resolve_maude_owner("Farapulse"), "'as_of' must be given")
  # The message has to name what to pass, or a required argument is just a wall.
  expect_error(resolve_maude_owner("Farapulse"), "Sys.Date()", fixed = TRUE)
  expect_error(resolve_maude_owner("Farapulse"), "date_received", fixed = TRUE)
})

test_that("resolve_maude_owner detects a cycle rather than looping", {
  cyclic <- tibble::tibble(
    entity = c("A", "B"),
    parent = c("B", "A"),
    from = as.Date(c(NA, NA)),
    to = as.Date(c(NA, NA)),
    note = NA_character_,
    source = NA_character_
  )

  expect_error(
    resolve_maude_owner("A", as_of = Sys.Date(), ownership = cyclic),
    "contains a cycle"
  )
})

test_that("as_of is recycled against entity", {
  expect_identical(
    resolve_maude_owner(
      c("Farapulse", "Physio-Control"),
      as_of = as.Date("2005-01-01")
    ),
    c("Farapulse", "Medtronic")
  )
  expect_length(resolve_maude_owner(character(0), as_of = Sys.Date()), 0L)
})

test_that("no pattern shadows another on real MAUDE strings", {
  # The failure a `priority` column exists to prevent: a general pattern sitting
  # above a specific one claims its strings, and nothing errors. `CRYOCATH` has
  # to be tested before `MEDTRONIC`, because reports arrive as
  # "MEDTRONIC CRYOCATH LP".
  cases <- maude_strings()

  manufacturers <- cases[cases$field == "manufacturer", ]
  expect_identical(
    normalize_maude_manufacturer(manufacturers$string),
    manufacturers$expected
  )

  brands <- cases[cases$field == "brand", ]
  expect_identical(
    normalize_maude_ablation(brands$string)$platform,
    brands$expected
  )
})

test_that("the fixture covers the traps it exists for", {
  # A fixture that quietly lost its interesting rows would pass the test above
  # while checking nothing.
  cases <- maude_strings()

  expect_true("MEDTRONIC CRYOCATH LP" %in% cases$string)
  expect_true("ARCTIC SUN 5000" %in% cases$string)
  expect_true("FARADRIVE STEERABLE SHEATH CLEAR" %in% cases$string)
  expect_true("VENUSA DE MEXICO S.A. DE C.V." %in% cases$string)
  expect_gt(sum(is.na(cases$expected)), 0)
})

test_that("the tables carry the coverage they were last measured at", {
  # Documented as an attribute rather than as a number in the roxygen, which
  # would go stale on the next CSV edit.
  for (index in list(maude_manufacturer_index, maude_ablation_index)) {
    coverage <- attr(index, "coverage")
    expect_s3_class(coverage, "tbl_df")
    expect_true(all(
      c("stream", "mentions", "matched", "measured_on") %in% names(coverage)
    ))
    expect_true(all(coverage$matched > 0 & coverage$matched <= 1))
    expect_s3_class(coverage$measured_on, "Date")
  }
})
