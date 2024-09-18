#' Calculate distances between 'LLM' embeddings from package text and function
#' definitions.
#'
#' The embeddings are currently retrieved from a local 'ollama' server running
#' Jina AI embeddings.
#'
#' @param packages Names of, or paths to,  one or more packages for which
#' embedding similarities are to be calculated.
#' @return A `data.frame` of pair-wise similarities between all packages
#' specified in `packages`.
#' @export
pkgsimil_embeddings <- function (packages = NULL) {

    pkgs_full <- packages
    packages <- convert_paths_to_pkgs (pkgs_full)

    cli::cli_inform ("Getting text embeddings ...")
    txt <- lapply (pkgs_full, function (p) get_pkg_text (p))
    embeddings <- get_embeddings (txt, code = FALSE)
    embeddings_text <- embeddings_to_dists (embeddings, packages)
    names (embeddings_text) [3] <- "d_text"

    cli::cli_inform ("Getting code embeddings ...")
    fns <- vapply (pkgs_full, function (p) get_pkg_code (p), character (1L))
    embeddings <- get_embeddings (txt, code = TRUE)
    embeddings_code <- embeddings_to_dists (embeddings, packages)
    names (embeddings_code) [3] <- "d_code"

    dplyr::left_join (embeddings_text, embeddings_code, by = c ("from", "to"))
}

convert_paths_to_pkgs <- function (packages) {
    is_installed <- pkg_is_installed (packages)
    if (any (is_installed) && !all (is_installed)) {
        stop (
            "packages must either name installed packages, ",
            "or supply paths to local source packages, but ",
            "not both."
        )
    }
    if (!any (is_installed)) {
        packages <- basename (packages)
    }
    return (packages)
}

#' Return raw 'LLM' embeddings from package text and function definitions.
#'
#' The embeddings are currently retrieved from a local 'ollama' server running
#' Jina AI embeddings.
#'
#' @inheritParams pkgsimil_embeddings
#' @return A list of two matrices of embeddings: one for the text descriptions
#' of the specified packages, including individual descriptions of all package
#' functions, and one for the entire code base.
#' @export
pkgsimil_embeddings_raw <- function (packages = NULL) {

    pkgs_full <- packages
    packages <- convert_paths_to_pkgs (pkgs_full)

    cli::cli_inform ("Getting text embeddings ...")
    txt <- lapply (pkgs_full, function (p) get_pkg_text (p))
    embeddings_text <- get_embeddings (txt, code = FALSE)

    cli::cli_inform ("Getting code embeddings ...")
    fns <- vapply (pkgs_full, function (p) get_pkg_code (p), character (1L))
    embeddings_code <- get_embeddings (txt, code = TRUE)

    colnames (embeddings_text) <- colnames (embeddings_code) <- packages

    list (text = embeddings_text, code = embeddings_code)
}

get_embeddings <- function (txt, code = FALSE) {
    if (!opt_is_quiet () && length (txt) > 100) {
        embeddings <- pbapply::pblapply (txt, function (i) get_embeddings_from_ollama (i, code = code))
    } else {
        embeddings <- lapply (txt, function (i) get_embeddings_from_ollama (i, code = code))
    }

    do.call (cbind, embeddings)
}

get_embeddings_from_ollama <- function (input, code = FALSE) {

    stopifnot (length (input) == 1L)

    u <- "http://localhost:11434/api/embeddings"

    model <- ifelse (
        code,
        "ordis/jina-embeddings-v2-base-code",
        "jina/jina-embeddings-v2-base-en"
    )
    data <- list (model = model, prompt = input)

    req <- httr2::request (u) |>
        httr2::req_headers ("Content-Type" = "application/json") |>
        httr2::req_body_json (data = data)

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    embeddings <- httr2::resp_body_json (resp, simplifyVector = FALSE)
    unlist (embeddings$embedding)
}

embeddings_to_dists <- function (embeddings, nms) {
    out <- stats::dist (t (embeddings))

    n <- length (nms)
    i <- seq_len (n - 1)
    j <- i + 1
    i <- rep (i, each = n - 1)
    j <- rep (j, times = n - 1)
    index <- which (j > i)
    i <- i [index]
    j <- j [index]

    data.frame (
        from = nms [i],
        to = nms [j],
        d = as.vector (out)
    )
}
