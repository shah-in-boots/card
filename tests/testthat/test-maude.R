test_that("load_maude_codes returns annex E (clinical signs)", {
  e <- load_maude_codes("E")
  expect_s3_class(e, "tbl_df")
  expect_true(nrow(e) > 0)
  expect_true(all(c("term", "definition", "fda_code", "imdrf_code") %in% names(e)))
  expect_true("Arrhythmia" %in% e$term)
})

test_that("load_maude_codes returns annex F (health impact)", {
  f <- load_maude_codes("F")
  expect_s3_class(f, "tbl_df")
  expect_true(nrow(f) > 0)
  expect_true(all(c("term", "definition", "fda_code", "imdrf_code") %in% names(f)))
})

test_that("load_maude_codes returns annex A (device problems)", {
  a <- load_maude_codes("A")
  expect_s3_class(a, "tbl_df")
  expect_true(nrow(a) > 0)
})

test_that("load_maude_codes rejects invalid annex", {
  expect_error(load_maude_codes("Z"), "Invalid annex")
})

test_that("evaluate_maude_event validates inputs", {
  skip_if_not_installed("ellmer")

  expect_error(
    evaluate_maude_event("clinical", event_text = "text", chat = "not_a_chat"),
    regexp = "problem_code"
  )

  expect_error(
    evaluate_maude_event("clinical", problem_code = "Arrhythmia", chat = "x"),
    regexp = "event_text"
  )

  expect_error(
    evaluate_maude_event("clinical", problem_code = "Arrhythmia",
                         event_text = "", chat = "x"),
    regexp = "event_text"
  )

  expect_error(
    evaluate_maude_event("clinical", problem_code = "Arrhythmia",
                         event_text = NA_character_, chat = "x"),
    regexp = "event_text"
  )

  expect_error(
    evaluate_maude_event("clinical", problem_code = "Arrhythmia",
                         event_text = "some text", chat = "not_a_chat"),
    regexp = "ellmer Chat object"
  )
})

test_that("evaluate_maude_event parses semicolon-separated problem codes", {
  skip_if_not_installed("ellmer")

  # We can test the parsing logic without actually calling the LLM
  # by checking that the function fails at the chat step, not the parsing step
  mock_chat <- structure(list(), class = "Chat")
  mock_chat$clone <- function() mock_chat
  mock_chat$set_system_prompt <- function(x) invisible(NULL)
  mock_chat$chat_structured <- function(...) {
    list(assessments = list(
      list(problem = "Arrhythmia", supported = TRUE, confidence = "high"),
      list(problem = "Chest Pain", supported = FALSE, confidence = "low")
    ))
  }

  result <- evaluate_maude_event(
    event_type = "clinical",
    problem_code = "Arrhythmia; Chest Pain",
    event_text = "Patient experienced arrhythmia during procedure.",
    chat = mock_chat
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$problem, c("Arrhythmia", "Chest Pain"))
  expect_equal(names(result), c("problem", "supported", "confidence"))
  expect_type(result$supported, "logical")
  expect_true(all(result$confidence %in% c("high", "medium", "low")))
})
