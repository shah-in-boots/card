#' Query gene associations from online databases
#'
#' @description
#' Retrieve gene associations for a disease or phenotype from supported
#' databases. The function is designed so that additional data sources can be
#' added in the future. Currently, the Open Targets Platform is supported.
#'
#' @param disease A non-empty character string describing the disease or
#'   phenotype of interest. The value is matched against entries in the
#'   selected database.
#' @param database A character string identifying the database to query.
#'   Defaults to `"open_targets"`. Additional databases may be supported in
#'   future versions.
#' @param limit A positive integer giving the maximum number of gene
#'   associations to return.
#' @param request_fun Optional. A function used to perform HTTP requests. This
#'   argument is primarily intended for testing; it should accept a GraphQL
#'   query string and a named list of variables and must return the parsed
#'   content of the `data` field.
#'
#' @returns A tibble with one row per gene target containing the matched disease
#'   identifier and name, the Open Targets target identifiers, the overall
#'   association score, a list column of data-source specific scores, and a set
#'   of columns for each evidence data type score reported by Open Targets.
#'
#' @examples
#' \donttest{
#' # Retrieve the top 25 genes associated with atrial fibrillation
#' genes <- query_gene_database("atrial fibrillation", limit = 25)
#' }
#'
#' @export
query_gene_database <- function(disease,
                                database = c("open_targets"),
                                limit = 100,
                                request_fun = NULL) {
  if (missing(disease) || !is.character(disease) || length(disease) != 1L) {
    stop("`disease` must be a length-one character string.")
  }

  disease <- trimws(disease)

  if (identical(disease, "")) {
    stop("`disease` must not be an empty string.")
  }

  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit <= 0) {
    stop("`limit` must be a single, positive integer value.")
  }

  limit <- as.integer(limit)

  database <- match.arg(tolower(database), choices = c("open_targets"))

  if (is.null(request_fun)) {
    request_fun <- .opentargets_request
  }

  if (identical(database, "open_targets")) {
    return(.query_opentargets(disease = disease, limit = limit, request_fun = request_fun))
  }

  stop("The requested database is not supported.")
}

.opentargets_endpoint <- "https://api.platform.opentargets.org/api/v4/graphql"

.opentargets_search_query <- "\
query searchDisease($term: String!, $size: Int) {\n  search(query: $term, entityNames: [DISEASE], page: {size: $size}) {\n    hits {\n      id\n      name\n      entity\n      score\n    }\n  }\n}\n"

.opentargets_disease_query <- "\
query diseaseAssociations($efoId: String!, $size: Int!, $cursor: String) {\n  disease(efoId: $efoId) {\n    id\n    name\n    associatedTargets(page: {size: $size, cursor: $cursor}) {\n      count\n      edges {\n        node {\n          score\n          target {\n            id\n            approvedSymbol\n            approvedName\n            geneId\n          }\n          datasourceScores {\n            id\n            datasourceId\n            score\n          }\n          datatypeScores {\n            id\n            datatypeId\n            score\n          }\n        }\n      }\n      pageInfo {\n        cursor\n        hasNextPage\n      }\n    }\n  }\n}\n"

.query_opentargets <- function(disease, limit, request_fun) {
  search_data <- request_fun(
    query = .opentargets_search_query,
    variables = list(term = disease, size = 50L)
  )

  hits <- search_data$search$hits

  hits <- .filter_disease_hits(hits)

  if (length(hits) == 0) {
    stop("No matching disease was found in the Open Targets Platform.")
  }

  best_hit <- .select_best_hit(hits)
  disease_id <- .extract_first_non_null(best_hit, c("id"))
  disease_name <- .extract_first_non_null(best_hit, c("name"))

  if (is.na(disease_id) || is.na(disease_name)) {
    stop("The Open Targets response did not contain the expected identifiers.")
  }

  results <- list()
  cursor <- NULL
  retrieved <- 0L

  repeat {
    page_size <- min(200L, limit - retrieved)

    payload <- request_fun(
      query = .opentargets_disease_query,
      variables = list(efoId = disease_id, size = page_size, cursor = cursor)
    )

    disease_payload <- payload$disease

    if (is.null(disease_payload$associatedTargets$edges) ||
        length(disease_payload$associatedTargets$edges) == 0) {
      break
    }

    page_edges <- disease_payload$associatedTargets$edges

    page_rows <- lapply(
      page_edges,
      .opentargets_edge_to_row,
      disease_id = disease_payload$id,
      disease_name = disease_payload$name
    )

    results <- c(results, page_rows)
    retrieved <- length(results)

    if (retrieved >= limit) {
      break
    }

    page_info <- disease_payload$associatedTargets$pageInfo

    if (!isTRUE(page_info$hasNextPage)) {
      break
    }

    cursor <- page_info$cursor
  }

  if (length(results) == 0) {
    return(.empty_opentargets_result(disease_id, disease_name))
  }

  results <- results[seq_len(min(limit, length(results)))]

  tibble::as_tibble(dplyr::bind_rows(results))
}

.opentargets_request <- function(query, variables) {
  response <- httr::POST(
    url = .opentargets_endpoint,
    httr::add_headers(`User-Agent` = "card R package", Accept = "application/json"),
    body = list(query = query, variables = variables),
    encode = "json"
  )

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    stop("Open Targets API request failed with status code ", status, ".")
  }

  parsed <- httr::content(response, as = "parsed", type = "application/json")

  if (!is.null(parsed$errors) && length(parsed$errors) > 0) {
    message <- .extract_first_non_null(parsed$errors[[1]], c("message"))
    if (is.na(message)) {
      message <- "Open Targets API returned an unspecified error."
    }
    stop(message)
  }

  parsed$data
}

.filter_disease_hits <- function(hits) {
  if (is.null(hits) || length(hits) == 0) {
    return(list())
  }

  Filter(function(hit) {
    entity <- .extract_first_non_null(hit, c("entity"))
    if (is.na(entity)) {
      return(FALSE)
    }
    identical(tolower(entity), "disease")
  }, hits)
}

.select_best_hit <- function(hits) {
  if (length(hits) == 1) {
    return(hits[[1]])
  }

  scores <- vapply(hits, function(hit) {
    score <- .extract_first_non_null(hit, c("score"))
    if (is.na(score)) {
      return(-Inf)
    }
    as.numeric(score)
  }, numeric(1))

  hits[[which.max(scores)]]
}

.opentargets_edge_to_row <- function(edge, disease_id, disease_name) {
  node <- edge$node
  target <- node$target

  datasource_scores <- .format_datasource_scores(node$datasourceScores)
  datatype_columns <- .format_datatype_scores(node$datatypeScores)

  tibble::as_tibble(
    dplyr::bind_cols(
      tibble::tibble(
        disease_id = if (!is.null(disease_id)) disease_id else NA_character_,
        disease_name = if (!is.null(disease_name)) disease_name else NA_character_,
        target_id = .extract_first_non_null(target, c("id")),
        gene_symbol = .extract_first_non_null(target, c("approvedSymbol", "symbol")),
        gene_name = .extract_first_non_null(target, c("approvedName", "name")),
        gene_ensembl_id = .extract_first_non_null(target, c("geneId", "ensemblId")),
        association_score = .safe_numeric(node$score),
        datasource_scores = list(datasource_scores)
      ),
      datatype_columns
    )
  )
}

.format_datasource_scores <- function(scores) {
  if (is.null(scores) || length(scores) == 0) {
    return(tibble::tibble(datasource_id = character(), score = numeric()))
  }

  tibble::tibble(
    datasource_id = vapply(scores, .extract_first_non_null, character(1), keys = c("id", "datasourceId")),
    score = vapply(scores, function(x) .safe_numeric(x$score), numeric(1))
  )
}

.format_datatype_scores <- function(scores) {
  if (is.null(scores) || length(scores) == 0) {
    return(tibble::tibble())
  }

  ids <- vapply(scores, .extract_first_non_null, character(1), keys = c("id", "datatypeId"))
  values <- vapply(scores, function(x) .safe_numeric(x$score), numeric(1))

  names(values) <- paste0("datatype_", ids)

  tibble::as_tibble(as.list(values))
}

.safe_numeric <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  as.numeric(x)
}

.extract_first_non_null <- function(x, keys) {
  for (key in keys) {
    if (!is.null(x[[key]])) {
      value <- x[[key]]
      if (is.character(value) || is.numeric(value) || is.logical(value)) {
        return(value)
      }
    }
  }
  NA
}

.empty_opentargets_result <- function(disease_id, disease_name) {
  tibble::tibble(
    disease_id = if (!is.null(disease_id)) disease_id else NA_character_,
    disease_name = if (!is.null(disease_name)) disease_name else NA_character_,
    target_id = character(),
    gene_symbol = character(),
    gene_name = character(),
    gene_ensembl_id = character(),
    association_score = numeric(),
    datasource_scores = list(),
    datatype_genetic_association = numeric(),
    datatype_somatic_mutation = numeric(),
    datatype_known_drug = numeric(),
    datatype_pathway = numeric(),
    datatype_rna_expression = numeric(),
    datatype_animal_model = numeric(),
    datatype_literature = numeric()
  )
}
