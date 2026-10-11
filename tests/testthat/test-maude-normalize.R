test_that("plants and spelling variants collapse onto one entity", {
  expect_identical(
    normalize_maude_manufacturer(c(
      "BOSTON SCIENTIFIC CORPORATION",
      "BOSTON SCIENTIFIC - MAPLE GROVE",
      "BOSTON SCIENTIFIC DE COSTA RICA S.R.L."
    )),
    rep("Boston Scientific", 3)
  )

  expect_identical(
    normalize_maude_manufacturer(c(
      "BIOSENSE WEBSTER INC",
      "BIOSENSE WEBSTER, INC. (JUAREZ)",
      "BIOSENSE WEBSTER, INC (IRWINDALE)"
    )),
    rep("Biosense Webster", 3)
  )

  # MPRI and IPG MFG SWITZERLAND name no company at all and were identified
  # from the brands they report.
  expect_identical(
    normalize_maude_manufacturer(c("MPRI", "IPG MFG SWITZERLAND")),
    rep("Medtronic", 2)
  )
})

test_that("an undated relationship resolves as an alias", {
  # An ownership row requires a closing date. A contract manufacturer is not an
  # acquisition and has none, so the string is an alias of the entity whose
  # product it builds -- VENUSA names neither Abbott nor St. Jude anywhere.
  expect_identical(
    normalize_maude_manufacturer("VENUSA DE MEXICO S.A. DE C.V."),
    "Abbott"
  )

  # Stockert builds generators for Biosense Webster on the same terms.
  expect_identical(normalize_maude_manufacturer("STOCKERT GMBH"), "Biosense Webster")
})

test_that("an acquired entity keeps its own name until it is resolved", {
  # The split that lets one acquisition be one row: `normalize_*()` says which
  # company made the device, `resolve_*()` says who owns that company.
  expect_identical(
    normalize_maude_manufacturer(c("FARAPULSE, INC.", "ST. JUDE MEDICAL")),
    c("Farapulse", "St. Jude Medical")
  )

  expect_identical(
    resolve_maude_owner(
      c("Farapulse", "St. Jude Medical"),
      as_of = as.Date("2024-01-01")
    ),
    c("Boston Scientific", "Abbott")
  )
})

test_that("a specific alias is not claimed by the parent's general one", {
  # "MEDTRONIC CRYOCATH LP" contains "MEDTRONIC", and CryoCath wins because it
  # sits below Medtronic in the ownership graph. Both resolve to Medtronic
  # today, which is why this would go unnoticed if it broke.
  expect_identical(
    normalize_maude_manufacturer("MEDTRONIC CRYOCATH LP"),
    "CryoCath"
  )
  expect_identical(
    resolve_maude_owner("CryoCath", as_of = as.Date("2024-01-01")),
    "Medtronic"
  )
  expect_identical(
    resolve_maude_owner("CryoCath", as_of = as.Date("2005-01-01")),
    "CryoCath"
  )
})

test_that("an unknown manufacturer is missing rather than guessed", {
  expect_identical(
    normalize_maude_manufacturer(c(
      "SOME COMPANY NOT IN THE INDEX",
      NA_character_,
      ""
    )),
    rep(NA_character_, 3)
  )

  # One row out per row in, whatever the input length.
  expect_length(normalize_maude_manufacturer(character(0)), 0L)
})

test_that("ablation modality comes from the brand rather than the product code", {
  out <- normalize_maude_ablation(c(
    "ARCTIC FRONT ADVANCE PRO CARDIAC CRYOABLATION CATHETER",
    "POLARX FIT",
    "FARAWAVE PULSED FIELD ABLATION CATHETER",
    "VARIPULSE BI-DIRECTIONAL CATHETER",
    "THERMOCOOL SMARTTOUCH SF",
    "TACTICATH QUARTZ CONTACT FORCE ABLATION CATHETER, 75MM",
    "HEARTLIGHT"
  ))

  expect_s3_class(out, "tbl_df")
  expect_identical(
    out$modality,
    c(
      "cryoablation", "cryoablation", "pulsed field", "pulsed field",
      "radiofrequency", "radiofrequency", "laser"
    )
  )

  # PolarX is the case that motivated the table: a cryoballoon that a
  # product-code rule falling back to radiofrequency puts in the wrong arm.
  expect_identical(out$platform[[2]], "PolarX")
})

test_that("the ablation index names the maker, not the owner", {
  # Hardcoding the parent here is what would have to be edited by hand on every
  # acquisition, and what let the two tables drift.
  out <- normalize_maude_ablation(c("FARAWAVE", "TACTICATH QUARTZ"))
  expect_identical(out$entity, c("Farapulse", "St. Jude Medical"))
  expect_false("manufacturer" %in% names(out))

  # The parent is one call away, and it says which date it used.
  expect_identical(
    resolve_maude_owner(out$entity, as_of = as.Date("2015-01-01")),
    c("Farapulse", "St. Jude Medical")
  )
  expect_identical(
    resolve_maude_owner(out$entity, as_of = as.Date("2024-01-01")),
    c("Boston Scientific", "Abbott")
  )
})

test_that("'arctic' alone is not a cryoablation signal", {
  # ARCTIC SUN is a targeted temperature management console. A "cryo|arctic"
  # brand rule claims it, and claims nothing about ablation by doing so.
  out <- normalize_maude_ablation(c("ARCTIC SUN 5000", "ARCTIC SUN STAT"))
  expect_true(all(is.na(out$modality)))
  expect_true(all(is.na(out$platform)))
})

test_that("devices that do not ablate carry no modality", {
  # Mapping, access and irrigation devices share the ablation product codes, so
  # they must be recognised and left without a modality rather than swept into
  # an arm.
  out <- normalize_maude_ablation(c(
    "PENTARAY NAV HIGH-DENSITY MAPPING ECO CATHETER",
    "OCTARAY MAPPING CATHETER",
    "FARADRIVE STEERABLE SHEATH CLEAR",
    "ENSITE MULTI-ELECTRODE ARRAY CATHETER"
  ))

  expect_true(all(is.na(out$modality)))
  expect_identical(
    out$platform,
    c("Pentaray", "Octaray", "Faradrive", "EnSite")
  )

  # FARADRIVE is the sheath that delivers the Farawave catheter. Matching it on
  # "FARA" as pulsed field would count the sheath as an ablation.
  expect_identical(out$entity[[3]], "Farapulse")
})

test_that("the longer alias wins where neither entity owns the other", {
  # Nothing relates these two, so the tie is broken by how much of the string
  # each alias explains.
  platforms <- tibble::tribble(
    ~platform,   ~aliases,     ~modality,       ~entity,
    "Too Broad", "arctic",     "cryoablation",  "Medtronic",
    "Not This",  "arctic sun", NA_character_,   "Medivance"
  )

  out <- normalize_maude_ablation("ARCTIC SUN 5000", platforms = platforms)
  expect_identical(out$platform, "Not This")
  expect_identical(out$modality, NA_character_)
})

test_that("a subsidiary is matched ahead of its parent whatever the row order", {
  # The ownership graph supplies the precedence, so sorting the registry
  # alphabetically cannot change what a string resolves to.
  entities <- maude_entities()
  shuffled <- entities[order(entities$entity), ]

  expect_identical(
    normalize_maude_manufacturer("MEDTRONIC CRYOCATH LP", entities = shuffled),
    "CryoCath"
  )
})

test_that("the lookups refuse a registry missing its columns", {
  expect_error(
    normalize_maude_manufacturer("MEDTRONIC", entities = tibble::tibble(x = 1)),
    "'entities' must be a data frame with the columns entity, aliases, parent"
  )
  expect_error(
    normalize_maude_ablation(
      "FARAWAVE",
      platforms = tibble::tibble(platform = "Farapulse", aliases = "farawave")
    ),
    "'platforms' must be a data frame"
  )
  expect_error(
    resolve_maude_owner(
      "Farapulse",
      as_of = Sys.Date(),
      entities = tibble::tibble(entity = "Farapulse")
    ),
    "'entities' must be a data frame"
  )
})
