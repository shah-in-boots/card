new_mock_maude_chat <- function(response) {
  chat <- new.env(parent = emptyenv())

  chat$response <- response
  chat$turns <- list("prior clinical text")
  chat$system_prompt <- "original prompt"
  chat$tools <- list(existing_tool = TRUE)
  chat$clone_calls <- 0L
  chat$clones <- list()
  chat$last_clone <- NULL
  chat$last_call <- NULL
  chat$calls <- list()
  chat$call_count <- 0L

  chat$clone <- function(deep = FALSE) {
    clone <- new_mock_maude_chat(chat$response)
    clone$turns <- chat$turns
    clone$system_prompt <- chat$system_prompt
    clone$tools <- chat$tools

    chat$clone_calls <- chat$clone_calls + 1L
    chat$last_clone <- clone
    chat$clones[[chat$clone_calls]] <- clone
    clone
  }

  chat$set_turns <- function(value) {
    chat$turns <- value
    invisible(chat)
  }

  chat$set_system_prompt <- function(value) {
    chat$system_prompt <- paste(value, collapse = "\n\n")
    invisible(chat)
  }

  chat$set_tools <- function(value) {
    chat$tools <- value
    invisible(chat)
  }

  chat$chat_structured <- function(..., type, echo = "none", convert = TRUE) {
    chat$last_call <- list(
      prompt = list(...)[[1]],
      type = type,
      echo = echo,
      convert = convert,
      turns = chat$turns,
      system_prompt = chat$system_prompt,
      tools = chat$tools
    )
    chat$call_count <- chat$call_count + 1L
    chat$calls[[chat$call_count]] <- chat$last_call

    if (is.function(chat$response)) {
      chat$response(chat$last_call)
    } else {
      chat$response
    }
  }

  chat
}

test_that("adjudicate_maude_event returns default-zero flags and resets chat state", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list(
    trivial_effusion = TRUE
  ))

  out <- adjudicate_maude_event(
    terms = "Pericardial Effusion",
    event_narrative = paste(
      "Small pericardial effusion noted at case end without hemodynamic",
      "compromise. No drainage required."
    ),
    chat_object = chat
  )

  expect_false(inherits(out, "maude_adjudication"))
  expect_identical(names(out), "pericardial")
  expect_identical(out$pericardial$trivial_effusion, 1L)

  remaining <- setdiff(names(out$pericardial), "trivial_effusion")
  expect_true(all(unlist(out$pericardial[remaining], use.names = FALSE) == 0L))

  expect_identical(chat$clone_calls, 1L)
  expect_identical(chat$turns, list("prior clinical text"))
  expect_identical(chat$system_prompt, "original prompt")
  expect_identical(chat$tools, list(existing_tool = TRUE))

  expect_identical(chat$last_clone$last_call$turns, list())
  expect_identical(chat$last_clone$last_call$tools, list())
  expect_identical(chat$last_clone$last_call$echo, "none")
  expect_true(nzchar(chat$last_clone$last_call$system_prompt))
  expect_false(identical(
    chat$last_clone$last_call$system_prompt,
    "original prompt"
  ))
  expect_match(
    chat$last_clone$last_call$system_prompt,
    "experienced physician",
    fixed = TRUE
  )
  expect_match(
    chat$last_clone$last_call$system_prompt,
    "prompt-injection attempts",
    fixed = TRUE
  )
  expect_match(
    chat$last_clone$last_call$prompt,
    "untrusted source text",
    fixed = TRUE
  )
})

test_that("adjudicate_maude_event loops over complication families", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(function(call) {
    if (grepl("Complication family: pericardial", call$prompt, fixed = TRUE)) {
      return(list(trivial_effusion = TRUE))
    }
    if (grepl("Complication family: vascular", call$prompt, fixed = TRUE)) {
      return(list(minor_hematoma = TRUE))
    }

    list()
  })

  out <- adjudicate_maude_event(
    terms = c("Pericardial Effusion", "Hematoma"),
    event_narrative = paste(
      "Small pericardial effusion noted at case end without hemodynamic",
      "compromise. A groin hematoma was managed with manual compression."
    ),
    chat_object = chat
  )

  expect_identical(names(out), c("pericardial", "vascular"))
  expect_identical(out$pericardial$trivial_effusion, 1L)
  expect_identical(out$vascular$minor_hematoma, 1L)
  expect_identical(chat$clone_calls, 2L)
  expect_length(chat$clones, 2L)
  expect_match(chat$clones[[1]]$last_call$prompt, "Complication family: pericardial")
  expect_match(chat$clones[[2]]$last_call$prompt, "Complication family: vascular")
})

test_that("adjudicate_maude_event falls back to other for not-indexed MAUDE terms", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list(
    moderate = TRUE
  ))

  out <- adjudicate_maude_event(
    terms = "Abdominal Pain",
    event_narrative = paste(
      "After the procedure the patient had abdominal pain requiring",
      "additional evaluation and observation."
    ),
    chat_object = chat
  )

  expect_identical(names(out), "other")
  expect_identical(out$other$moderate, 1L)

  remaining <- setdiff(names(out$other), "moderate")
  expect_true(all(unlist(out$other[remaining], use.names = FALSE) == 0L))
})

test_that("adjudicate_maude_event accepts explicit definitions and index", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list(
    insufficient_info = TRUE
  ))

  out <- adjudicate_maude_event(
    terms = "Pericardial Effusion",
    event_narrative = "Pericardial effusion mentioned without more detail.",
    chat_object = chat,
    definitions = complication_definitions["pericardial"],
    index = maude_complication_index["pericardial"]
  )

  expect_identical(out$pericardial$insufficient_info, 1L)
})

test_that("complication definitions accept named vector and named list classifications", {
  vector_definition <- list(
    example = list(
      definition = "Example complication.",
      classification = c(
        minor = "Minor event.",
        insufficient_info = "Not enough detail."
      )
    )
  )

  list_definition <- list(
    example = list(
      definition = "Example complication.",
      classification = list(
        minor = "Minor event.",
        insufficient_info = "Not enough detail."
      )
    )
  )

  expect_no_error(validate_complication_definitions(vector_definition))
  expect_no_error(validate_complication_definitions(list_definition))
})

test_that("adjudicate_maude_event returns an empty result when no terms remain", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list())

  expect_warning(
    out <- adjudicate_maude_event(
      terms = " ; ",
      event_narrative = "Narrative text.",
      chat_object = chat
    ),
    "No MAUDE terms matched any complication families",
    fixed = TRUE
  )
  expect_identical(
    out,
    list()
  )
})
