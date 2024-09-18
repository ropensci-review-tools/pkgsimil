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
pkgsimil_embedding_dists <- function (packages = NULL) {

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
#' @inheritParams pkgsimil_embedding_dists
#' @param functions_only If `TRUE`, calculate embeddings for function
#' descriptions only. This is intended to generate a separate set of embeddings
#' which can then be used to match plain-text queries of functions, rather than
#' entire packages.
#' @return If `!functions_only`, a list of two matrices of embeddings: one for
#' the text descriptions of the specified packages, including individual
#' descriptions of all package functions, and one for the entire code base. For
#' `functions_only`, a single matrix of embeddings for all function
#' descriptions.
#' @export
pkgsimil_embeddings_raw <- function (packages = NULL, functions_only = FALSE) {

    pkgs_full <- packages
    packages <- convert_paths_to_pkgs (pkgs_full)

    cli::cli_inform ("Getting text embeddings ...")
    txt <- lapply (pkgs_full, function (p) get_pkg_text (p))

    if (!functions_only) {
        embeddings_text <- get_embeddings (txt, code = FALSE)

        cli::cli_inform ("Getting code embeddings ...")
        fns <- vapply (pkgs_full, function (p) get_pkg_code (p), character (1L))
        embeddings_code <- get_embeddings (txt, code = TRUE)

        colnames (embeddings_text) <- colnames (embeddings_code) <- packages

        ret <- list (text = embeddings_text, code = embeddings_code)
    } else {
        txt_fns <- get_all_fn_descs (txt)
        ret <- get_embeddings (txt_fns$desc, code = FALSE)
    }
    return (ret)
}

get_all_fn_descs <- function (txt) {
    fn_txt <- lapply (txt, function (i) {
        i_sp <- strsplit (i, "\\n") [[1]]
        ptn <- "^[[:space:]]*#[[:space:]]"
        pkg_name <- grep (ptn, i_sp)
        if (length (pkg_name) > 0L) {
            pkg_name <- gsub ("[[:space:]]*", "", gsub (ptn, "", i_sp [pkg_name [1]]))
        } else {
            pkg_name <- "pkg_has_no_name"
        }

        pos <- grep ("##\\s+Functions$", i_sp)
        if (length (pos) == 0) {
            fn_nms <- fn_desc <- character (0L)
        } else {
            # Fn defs are always added at end, so pos has to be last value:
            pos <- utils::tail (pos, n = 1L)

            fns <- i_sp [seq (pos + 1, length (i_sp))]
            index <- which (!nzchar (fns) | grepl ("^[[:space:]]+$", fns))
            if (length (index) > 0L) fns <- fns [-index]
            index1 <- grep ("^[[:space:]]*###", fns)
            index2 <- c (index1 [-1] - 1L, length (fns))
            index <- apply (
                cbind (index1, index2), 1,
                function (i) seq (i [1], i [2]),
                simplify = FALSE
            )
            fns <- lapply (index, function (i) fns [i])
            fn_nms <- vapply (fns, function (i) {
                gsub ("###|[[:space:]]*", "", i [1])
            }, character (1L))
            fn_nms <- paste0 (pkg_name, "::", fn_nms)
            fn_descs <- vapply (fns, function (i) {
                gsub ("^[[:space:]]*", "", paste0 (i [-1], collapse = " "))
            }, character (1L))
        }
        data.frame (fn = fn_nms, desc = fn_descs)
    })
    do.call (rbind, fn_txt)
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

#' Load embeddings generated by the \link{pkgsimil_embeddings_raw} function,
#' either for all rOpenSci packages or, if `fns = TRUE`,  all individual
#' functions within those packages.
#'
#' @param fns If `FALSE` (default), load embeddings for all rOpenSci packages;
#' otherwise load (considerably larger dataset of) embeddings for all
#' individual functions.
#' @export
pkgsimil_load_embeddings <- function (fns = FALSE) {
    fname <- ifelse (fns, "embeddings-fns.Rds", "embeddings.Rds")
    fname <- fs::path (pkgsimil_cache_path (), fname)
    if (!fs::file_exists (fname)) {
        fname <- pkgsimil_dl_embeddings (fns = fns)
    }
    readRDS (fname)
}

pkgsimil_dl_embeddings <- function (fns = FALSE) {

    url_base <- "https://github.com/ropensci-review-tools/pkgsimil/releases/download/"
    version <- "v0.1.2"
    file <- ifelse (fns, "embeddings-fns.Rds", "embeddings.Rds")
    url <- paste0 (url_base, version, "/", file)

    destfile <- fs::path (pkgsimil_cache_path (), file)
    download.file (url, destfile = destfile)
    return (destfile)
}
