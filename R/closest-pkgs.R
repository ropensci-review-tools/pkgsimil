#' Use the embeddings from \link{pkgsimil_embeddings_raw} to identify most
#' similar packages to a given local repository.
#'
#' @param input Either a path to local source code of an R package, or a text
#' string.
#' @param embeddings Large Language Model embeddings for all rOpenSci packages,
#' generated from \link{pkgsimil_embeddings_raw}.
#' @param n identify the `n` most similar packages in terms of both code and
#' text embeddings.
#' @return A `data.frame` of all packages in the embeddings data and
#' corresponding distances.
#' @export
pkgsimil_similar_pkgs <- function (input, embeddings, n = 5L) {
    stopifnot (is.list (embeddings))
    stopifnot (identical (names (embeddings), c ("text", "code")))
    stopifnot (fs::dir_exists (input))

    res <- similar_pkgs_from_pkg (input, embeddings, n)

    return (res)
}

similar_pkgs_from_pkg <- function (input, embeddings, n) {
    op <- options ()
    options (rlib_message_verbosity = "quiet")

    emb <- pkgsimil_embeddings_raw (input)

    nrow <- nrow (emb$text)
    npkgs <- ncol (embeddings$text)
    emb_text <- matrix (emb$text, nrow = nrow, ncol = npkgs)
    emb_code <- matrix (emb$code, nrow = nrow, ncol = npkgs)
    d_text <- colSums (sqrt ((emb_text - embeddings$text)^2))
    d_text <- data.frame (pkg = names (d_text), text = unname (d_text))
    d_code <- colSums (sqrt ((emb_code - embeddings$code)^2))
    d_code <- data.frame (pkg = names (d_code), code = unname (d_code))

    out <- dplyr::left_join (d_text, d_code, by = "pkg")
    out$code <- out$code / max (out$code)
    out$text <- out$text / max (out$text)

    index <- order (out$code)
    n <- 5L
    out_code <- out [index [seq_len (n)], c ("pkg", "code")]
    rownames (out_code) <- NULL

    index <- order (out$text)
    n <- 5L
    out_text <- out [index [seq_len (n)], c ("pkg", "text")]
    rownames (out_text) <- NULL

    options (op)

    list (text = out_text, code = out_code)
}
