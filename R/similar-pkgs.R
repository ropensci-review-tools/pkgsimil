#' Use the embeddings from \link{pkgsimil_embeddings_from_pkgs} to identify
#' most similar packages to a given local repository.
#'
#' @param input Either a path to local source code of an R package, or a text
#' string.
#' @param embeddings Large Language Model embeddings for all rOpenSci packages,
#' generated from \link{pkgsimil_embeddings_from_pkgs}. If not provided,
#' pre-generated embeddings will be downloaded and stored in a local cache
#' directory.
#' @param idfs Inverse Document Frequency tables for all rOpenSci packages,
#' generated from \link{bm25_idf}. If not provided, pre-generated IDF tables
#' will be downloaded and stored in a local cache directory.
#' @param input_is_code A binary flag indicating whether `input` is code or
#' plain text. Ignored if `input` is path to a local package; otherwise can be
#' used to force appropriate interpretation if input type.
#' @param n identify the `n` most similar packages in terms of both code and
#' text embeddings.
#' @return If `input` is a path to a local package, a list of two character
#' vectors naming the `n` most similar packages in terms of descriptive textual
#' similarity ("text"), and in terms of similarity of code structure ("code").
#' If `input` is a single text string, a single character vector is returned
#' naming the `n` most similar packages.
#'
#' @seealso input_is_code
#' @export
pkgsimil_similar_pkgs <- function (
    input,
    embeddings = NULL,
    idfs = NULL,
    input_is_code = text_is_code (input),
    n = 5L) {

    if (is.null (embeddings)) {
        embeddings <- pkgsimil_load_data ("embeddings")
    }
    if (is.null (idfs)) {
        # idfs <- pkgsimil_load_data ("idfs")
    }

    nms_expected <- c ("text_with_fns", "text_wo_fns", "code")
    stopifnot (is.list (embeddings))
    stopifnot (identical (names (embeddings), nms_expected))
    # stopifnot (is.list (idfs))
    # stopifnot (identical (names (idfs), c ("idfs", "token_lists")))

    if (fs::dir_exists (input)) {
        res <- similar_pkgs_from_pkg (input, embeddings, n)
        res <- lapply (res, function (i) i$pkg)
    } else {
        res <- similar_pkgs_from_text (input, embeddings, input_is_code, n)$pkg
    }

    return (res)
}

similar_pkgs_from_pkg <- function (input, embeddings, n) {
    op <- options ()
    options (rlib_message_verbosity = "quiet")

    emb <- pkgsimil_embeddings_from_pkgs (input)

    options (op)

    npkgs <- ncol (embeddings$text_with_fns)
    nrow <- nrow (emb$text_with_fns)
    emb_text <- matrix (emb$text_with_fns, nrow = nrow, ncol = npkgs)
    emb_code <- matrix (emb$code, nrow = nrow, ncol = npkgs)
    d_text <- colSums (sqrt ((emb_text - embeddings$text_with_fns)^2))
    d_text <- data.frame (pkg = names (d_text), text = unname (d_text))
    d_code <- colSums (sqrt ((emb_code - embeddings$code)^2))
    d_code <- data.frame (pkg = names (d_code), code = unname (d_code))

    out <- dplyr::left_join (d_text, d_code, by = "pkg")
    out$code <- out$code / max (out$code)
    out$text <- out$text / max (out$text)

    list (
        text = order_output (out, "text", n),
        code = order_output (out, "code", n)
    )
}

#' Use the embeddings from \link{pkgsimil_embeddings_from_pkgs} with
#' `functions_only = TRUE` to identify functions best matching a given input
#' string.
#'
#' @inheritParams pkgsimil_similar_pkgs
#' @param input A text string.
#' @param embeddings A single matrix of embeddings produced from
#' \link{pkgsimil_embeddings_from_pkgs} with `functions_only = TRUE`. If not
#' cache directory.
#' provided, pre-generated embeddings will be downloaded and stored in a local
#' @return A character vector of function names in the form
#' "<package>::<function>".
#' @export
pkgsimil_similar_fns <- function (input, embeddings = NULL, n = 5L) {
    if (is.null (embeddings)) {
        embeddings <- pkgsimil_load_data ("embeddings", fns = TRUE)
    }
    stopifnot (is.matrix (embeddings))
    stopifnot (is.character (input))
    stopifnot (length (input) == 1L)

    op <- options ()
    options (rlib_message_verbosity = "quiet")
    emb <- get_embeddings (input)
    options (op)

    emb_mat <- matrix (emb, nrow = length (emb), ncol = ncol (embeddings))
    d <- colMeans (sqrt ((emb_mat - embeddings)^2))
    index <- order (d) [seq_len (n)]
    colnames (embeddings) [index]
}

order_output <- function (out, what = "text", n) {
    index <- order (out [[what]])
    out <- out [index [seq_len (n)], c ("pkg", what)]
    rownames (out) <- NULL

    return (out)
}

similar_pkgs_from_text <- function (
    input,
    embeddings = NULL,
    idfs = NULL,
    input_is_code = text_is_code (input),
    n = 5L) {

    stopifnot (is.character (input))
    stopifnot (length (input) == 1L)

    if (is.null (embeddings)) {
        embeddings <- pkgsimil_load_data ("embeddings")
    }
    if (input_is_code) {
        similarities <- similarity_embeddings (input, embeddings$code, input_is_code = TRUE)
    } else {
        similarities <- similarity_embeddings (input, embeddings, input_is_code = FALSE)
    }

    similarities_bm25 <- pkgsimil_bm25 (input, idfs = idfs)

    index <- seq_len (n)
    return (similarities [index, ])
}

similarity_embeddings <- function (input, embeddings, input_is_code) {

    this_emb <- get_embeddings (input, code = input_is_code)

    if (is.list (embeddings)) {

        dat_with_fns <- cosine_similarity (this_emb, embeddings$text_with_fns)
        dat_wo_fns <- cosine_similarity (this_emb, embeddings$text_wo_fns)

        names (dat_wo_fns) [2] <- "simil_wo_fns"
        names (dat_with_fns) [2] <- "simil_with_fns"

        dat <- dplyr::left_join (dat_with_fns, dat_wo_fns, by = "pkg")

    } else {

        dat <- cosine_similarity (this_emb, embeddings)
    }

    return (dat)
}

similarity_bm25 <- function (input, bm25) {

    this_emb <- get_embeddings (input, code = FALSE)
    b <- pkgsimil_bm25 (input)
}

#' cosine similarity between one input vector and an input matrix with column
#' names.
#' @noRd
cosine_similarity <- function (this_vec, this_mat) {

    nrow <- length (this_vec)
    ncol <- ncol (this_mat)
    emb_mat <- matrix (this_vec, nrow = nrow, ncol = ncol)

    cs_num <- colSums (emb_mat * this_mat)
    cs_denom <- sqrt (colSums (emb_mat^2) * colSums (this_mat^2))
    cs <- cs_num / cs_denom

    index <- order (cs, decreasing = TRUE)
    res <- data.frame (pkg = names (cs), simil = unname (cs)) [index, ]
    rownames (res) <- NULL

    return (res)
}
