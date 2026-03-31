new_mock_maude_chat <- function(response) {
  chat <- new.env(parent = emptyenv())

  chat$response <- response
  chat$turns <- list("prior clinical text")
  chat$system_prompt <- "original prompt"
  chat$tools <- list(existing_tool = TRUE)
  chat$clone_calls <- 0L
  chat$last_clone <- NULL
  chat$last_call <- NULL

  chat$clone <- function(deep = FALSE) {
    clone <- new_mock_maude_chat(chat$response)
    clone$turns <- chat$turns
    clone$system_prompt <- chat$system_prompt
    clone$tools <- chat$tools

    chat$clone_calls <- chat$clone_calls + 1L
    chat$last_clone <- clone
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

    chat$response
  }

  chat
}

test_that("adjudicate_maude_event returns default-zero flags and resets chat state", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list(
    pericardial = list(
      trivial_effusion = TRUE
    )
  ))

  out <- adjudicate_maude_event(
    terms = "Pericardial Effusion",
    event_narrative = paste(
      "Small pericardial effusion noted at case end without hemodynamic",
      "compromise. No drainage required."
    ),
    llm = chat
  )

  expect_s3_class(out, "maude_adjudication")
  expect_identical(attr(out, "selected_complications"), "pericardial")
  expect_identical(out$pericardial$trivial_effusion, 1L)

  remaining <- setdiff(names(out$pericardial), "trivial_effusion")
  expect_true(all(unlist(out$pericardial[remaining], use.names = FALSE) == 0L))

  expect_identical(chat$clone_calls, 1L)
  expect_identical(chat$turns, list("prior clinical text"))
  expect_identical(chat$system_prompt, "original prompt")
  expect_identical(chat$tools, list(existing_tool = TRUE))

  expect_identical(chat$last_clone$last_call$turns, list())
  expect_identical(chat$last_clone$last_call$tools, list())
  expect_true(nzchar(chat$last_clone$last_call$system_prompt))
  expect_false(identical(
    chat$last_clone$last_call$system_prompt,
    "original prompt"
  ))
  expect_match(
    chat$last_clone$last_call$prompt,
    "untrusted source text",
    fixed = TRUE
  )
})

test_that("adjudicate_maude_event falls back to other for not-indexed MAUDE terms", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list(
    other = list(
      moderate = TRUE
    )
  ))

  out <- adjudicate_maude_event(
    terms = "Abdominal Pain",
    event_narrative = paste(
      "After the procedure the patient had abdominal pain requiring",
      "additional evaluation and observation."
    ),
    llm = chat
  )

  expect_identical(attr(out, "selected_complications"), "other")
  expect_identical(out$other$moderate, 1L)

  remaining <- setdiff(names(out$other), "moderate")
  expect_true(all(unlist(out$other[remaining], use.names = FALSE) == 0L))
})

test_that("adjudicate_maude_event accepts legacy argument names", {
  skip_if_not_installed("ellmer")

  chat <- new_mock_maude_chat(list(
    pericardial = list(
      insufficient_info = TRUE
    )
  ))

  out <- adjudicate_maude_event(
    terms = "Pericardial Effusion",
    event_narrative = "Pericardial effusion mentioned without more detail.",
    llm = chat,
    complication_definitions = complication_definitions["pericardial"],
    complication_index = maude_complication_index["pericardial"]
  )

  expect_identical(out$pericardial$insufficient_info, 1L)
})
