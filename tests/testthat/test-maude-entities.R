# Invariants of the MAUDE entity and platform registries.
#
# The registries are curated by hand, so a bad edit is a typo away and would
# otherwise ship silently. Everything here runs offline.

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

test_that("no alias is claimed by two entities", {
  entities <- maude_entities()
  aliases <- unlist(
    strsplit(unique(entities[, c("entity", "aliases")])$aliases, "|", fixed = TRUE),
    use.names = FALSE
  )
  expect_false(anyDuplicated(trimws(aliases)) > 0)

  platforms <- maude_platforms()
  brand_aliases <- unlist(
    strsplit(platforms$aliases, "|", fixed = TRUE),
    use.names = FALSE
  )
  expect_false(anyDuplicated(trimws(brand_aliases)) > 0)
  expect_false(anyDuplicated(platforms$platform) > 0)
})

test_that("every entity's rows agree on the aliases that name it", {
  # An entity sold twice has one row per interval, and a spelling added to one
  # row but not the other would be found by only half of the lookups.
  entities <- maude_entities()
  by_entity <- split(entities$aliases, entities$entity)
  expect_true(all(vapply(by_entity, function(x) length(unique(x)) == 1L, logical(1))))
})

test_that("every platform carries an entity and a known modality", {
  platforms <- maude_platforms()
  known <- c(
    "cryoablation", "radiofrequency", "pulsed field",
    "pulsed field or radiofrequency", "laser", NA_character_
  )

  expect_false(anyNA(platforms$entity))
  expect_true(all(platforms$modality %in% known))

  # A platform naming an entity the registry does not know is a typo.
  entities <- maude_entities()
  known_entities <- unique(c(entities$entity, stats::na.omit(entities$parent)))
  expect_true(all(platforms$entity %in% known_entities))
})

test_that("every parent is either an entity or a terminal owner", {
  # The registry may name a holding company only as an owner -- Cardinal Health
  # never filed a MAUDE report -- but a parent that is a misspelling of an
  # entity would resolve to a company that does not exist.
  entities <- maude_entities()
  parents <- unique(stats::na.omit(entities$parent))
  strangers <- setdiff(parents, entities$entity)

  expect_setequal(
    strangers,
    c("Johnson & Johnson", "Cardinal Health", "Hellman & Friedman", "Opto Circuits")
  )
})

test_that("ownership intervals are well formed and do not overlap", {
  entities <- maude_entities()
  dated <- entities[!is.na(entities$parent), ]

  expect_true(all(
    is.na(dated$from) | is.na(dated$to) | dated$from < dated$to
  ))

  # An entity owned by two parents at once would make the walk pick one
  # silently, which is the failure a divestiture invites.
  overlapping <- vapply(
    split(dated, dated$entity),
    function(rows) {
      if (nrow(rows) < 2L) {
        return(FALSE)
      }
      starts <- dplyr::coalesce(rows$from, as.Date("1900-01-01"))
      ends <- dplyr::coalesce(rows$to, as.Date("2999-12-31"))
      any(outer(starts, ends, `<`) & outer(ends, starts, `>`) &
            !diag(TRUE, nrow(rows)))
    },
    logical(1)
  )
  expect_false(any(overlapping))
})

test_that("a dated row is either cited or says how approximate it is", {
  # A constant with no citation is assumed to be wrong, so an uncited closing
  # date has to declare itself rather than look settled.
  entities <- maude_entities()
  dated <- entities[!is.na(entities$from) | !is.na(entities$to), ]
  uncited <- dated[is.na(dated$source), ]

  expect_true(all(grepl("approximate", uncited$note, fixed = TRUE)))
})

test_that("the ownership graph resolves for every entity it knows", {
  entities <- maude_entities()
  known <- unique(c(entities$entity, stats::na.omit(entities$parent)))

  for (when in c("1992-01-01", "2010-06-15", format(Sys.Date()))) {
    expect_length(
      maude_resolve_owner(known, as_of = as.Date(when)),
      length(known)
    )
  }
})

test_that("maude_resolve_owner follows a chain rather than a single hop", {
  # Telectronics reaches Abbott through St. Jude, and nobody wrote Abbott next
  # to Telectronics.
  expect_identical(
    maude_resolve_owner("Telectronics Pacing Systems", as_of = as.Date("2000-01-01")),
    "St. Jude Medical"
  )
  expect_identical(
    maude_resolve_owner("Telectronics Pacing Systems", as_of = as.Date("2024-01-01")),
    "Abbott"
  )
})

test_that("maude_resolve_owner expresses a divestiture", {
  expect_identical(
    maude_resolve_owner(
      rep("Physio-Control", 3),
      as_of = as.Date(c("2005-01-01", "2014-01-01", "2020-01-01"))
    ),
    c("Medtronic", "Physio-Control", "Stryker")
  )
})

test_that("an entity sold on from one owner to the next resolves to each in turn", {
  # Cordis was Johnson & Johnson's, then Cardinal Health's, then Hellman &
  # Friedman's. A single-parent row would have named only the first, and every
  # report between 2015 and 2021 would have been counted against J&J.
  expect_identical(
    maude_resolve_owner(
      rep("Cordis", 3),
      as_of = as.Date(c("2010-01-01", "2018-01-01", "2024-01-01"))
    ),
    c("Johnson & Johnson", "Cardinal Health", "Hellman & Friedman")
  )
})

test_that("maude_resolve_owner refuses to guess a date", {
  expect_error(maude_resolve_owner("Farapulse"), "'as_of' must be given")
})

test_that("maude_resolve_owner detects a cycle rather than looping", {
  cyclic <- tibble::tribble(
    ~entity, ~aliases, ~parent, ~from, ~to,
    "A",     "a",      "B",     NA,    NA,
    "B",     "b",      "A",     NA,    NA
  )
  cyclic$from <- as.Date(cyclic$from)
  cyclic$to <- as.Date(cyclic$to)

  expect_error(
    maude_resolve_owner("A", as_of = Sys.Date(), entities = cyclic),
    "contains a cycle"
  )
})

test_that("as_of is recycled against entity", {
  expect_identical(
    maude_resolve_owner("St. Jude Medical", as_of = as.Date(c("2010-01-01", "2024-01-01"))),
    c("St. Jude Medical", "Abbott")
  )
})

test_that("no alias shadows another on real MAUDE strings", {
  # The failure the precedence rule exists to prevent: a general alias claims a
  # specific one's strings, and nothing errors. "MEDTRONIC CRYOCATH LP" is the
  # case that would break first.
  cases <- maude_strings()

  manufacturers <- cases[cases$field == "manufacturer", ]
  expect_identical(
    maude_normalize_manufacturer(manufacturers$string),
    manufacturers$expected
  )

  brands <- cases[cases$field == "brand", ]
  expect_identical(
    maude_normalize_ablation(brands$string)$platform,
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

test_that("every alias normalises back to the entity that declares it", {
  entities <- unique(maude_entities()[, c("entity", "aliases")])

  for (i in seq_len(nrow(entities))) {
    terms <- trimws(unlist(strsplit(entities$aliases[[i]], "|", fixed = TRUE)))
    plain <- gsub("^\\^|\\$$", "", terms)
    expect_identical(
      maude_normalize_manufacturer(plain),
      rep(entities$entity[[i]], length(plain)),
      info = entities$entity[[i]]
    )
  }
})

test_that("maude_search_terms returns every name a company has filed under", {
  # The direction that stops a query hiding reports: a filter on Abbott alone
  # misses two decades of St. Jude filings.
  abbott <- maude_search_terms("Abbott")

  expect_s3_class(abbott, "tbl_df")
  expect_named(abbott, c("entity", "term", "from", "to"))
  expect_true(all(
    c("St. Jude Medical", "Telectronics Pacing Systems", "CardioMEMS",
      "Irvine Biomedical", "Thoratec") %in% abbott$entity
  ))
  expect_true("abbott" %in% abbott$term)

  # The window is when Abbott owned the entity, not when St. Jude bought it.
  expect_identical(
    unique(abbott$from[abbott$entity == "Telectronics Pacing Systems"]),
    as.Date("2017-01-04")
  )

  # The terms are searchable strings, so an anchor never reaches openFDA.
  medtronic <- maude_search_terms("Medtronic")
  expect_true("mpri" %in% medtronic$term)
  expect_false(any(grepl("[$^]", medtronic$term)))
})

test_that("maude_search_terms respects the date it is given", {
  # Before the acquisition closed, St. Jude was not Abbott's to search for.
  early <- maude_search_terms("Abbott", as_of = as.Date("2016-01-01"))
  expect_setequal(early$entity, "Abbott")

  late <- maude_search_terms("Abbott", as_of = as.Date("2018-01-01"))
  expect_true("St. Jude Medical" %in% late$entity)
})

test_that("maude_search_terms does not claim an entity its company never owned", {
  # Physio-Control bought HeartSine in 2015, three years after Medtronic had
  # sold Physio-Control. An intersection of the two windows is empty, and the
  # entity must not be listed at all.
  expect_false("HeartSine Technologies" %in% maude_search_terms("Medtronic")$entity)
  expect_true("HeartSine Technologies" %in% maude_search_terms("Stryker")$entity)
})

test_that("maude_search_terms refuses a company the registry does not name", {
  expect_error(
    maude_search_terms("Not A Company"),
    "which the registry does not name"
  )
})

test_that("a manufacturer filter warns about the names it leaves out", {
  # `maude_query()` raises this while assembling the query, so the check runs
  # against the helper rather than against openFDA.
  expect_warning(
    warn_maude_family("ABBOTT"),
    "St\\. Jude Medical"
  )
  expect_warning(warn_maude_family("ABBOTT"), "maude_search_terms")

  # A company that has acquired nobody, and a string the registry cannot place,
  # both pass in silence.
  expect_no_warning(warn_maude_family("BIOTRONIK SE & CO KG"))
  expect_no_warning(warn_maude_family("SOME COMPANY NOT IN THE REGISTRY"))

  # Naming the family yourself is what the warning asks for, so doing it must
  # not then warn.
  expect_no_warning(warn_maude_family(maude_search_terms("Abbott")$term))
})

test_that("aliases match the ways MAUDE mangles a name", {
  # Every one of these is a real string from the openFDA count endpoint, and
  # each broke a different part of the matching rule.
  expect_identical(
    maude_normalize_manufacturer(c(
      "MEDTRONICS",                 # a trailing letter, so the alias carries *
      "ARROW INTL., INC.",          # abbreviated, and the commonest spelling
      "BAYLIS MEDICALE CIE INC"     # a word that continues past the alias
    )),
    c("Medtronic", "Arrow International", "Baylis Medical")
  )

  expect_identical(
    maude_normalize_ablation(c(
      "UNK_THERMOCOOL SF NAV",             # underscore, which `\\b` treats as a letter
      "POLARXFIT",                         # no separator at all
      "THERAPY\u00bf ABLATION CATHETER",  # a mangled trademark mark between words
      "NAVI-STAR THERMO-COOL"              # hyphens where another string has none
    ))$platform,
    c("Thermocool", "PolarX", "Therapy", "Thermocool")
  )

  # The boundary still holds where it protects a short alias.
  expect_identical(
    maude_normalize_manufacturer(c("LOMBARD MEDICAL", "BDX CORPORATION", "MPRIME LLC")),
    rep(NA_character_, 3)
  )
})

test_that("maude_search_terms accepts a company name in any case", {
  expect_identical(maude_search_terms("abbott"), maude_search_terms("Abbott"))

  # A name with no near match ends cleanly rather than with an empty clause
  expect_error(maude_search_terms("Nobody Inc"), "does not name\\.$")
  expect_error(maude_search_terms("Abbot"), "look close: Abbott")
})
