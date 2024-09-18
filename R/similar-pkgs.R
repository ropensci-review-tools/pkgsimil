#' Use the embeddings from \link{pkgsimil_embeddings_raw} to identify most
#' similar packages to a given local repository.
#'
#' @param input Either a path to local source code of an R package, or a text
#' string.
#' @param embeddings Large Language Model embeddings for all rOpenSci packages,
#' generated from \link{pkgsimil_embeddings_raw}.
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
pkgsimil_similar_pkgs <- function (input, embeddings, input_is_code = text_is_code (input), n = 5L) {
    stopifnot (is.list (embeddings))
    stopifnot (identical (names (embeddings), c ("text", "code")))

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

    emb <- pkgsimil_embeddings_raw (input)

    options (op)

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

    list (text = order_output (out, "text"), code = order_output (out, "code"))
}

#' Use the embeddings from \link{pkgsimil_embeddings_raw} with `functions_only
#' = TRUE` to identify functions best matching a given input string.
#'
#' @param input A text string.
#' @param embeddings A single matrix of embeddings produced from
#' \link{pkgsimil_embeddings_raw} with `functions_only = TRUE`.
#' @return A character vector of function names in the form
#' "<package>::<function>".
#' @export
pkgsimil_similar_fns <- function (input, embeddings, n = 5L) {
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

order_ouput <- function (out, what = "text", n) {
    index <- order (out [[what]])
    out <- out [index [seq_len (n)], c ("pkg", what)]
    rownames (out) <- NULL

    return (out)
}

similar_pkgs_from_text <- function (input, embeddings, input_is_code, n) {

    if (input_is_code) {
        embeddings <- embeddings$code
    } else {
        embeddings <- embeddings$text
    }

    this_emb <- get_embeddings (input, code = input_is_code)

    nrow <- length (this_emb)
    npkgs <- ncol (embeddings)
    emb_mat <- matrix (this_emb, nrow = nrow, ncol = npkgs)
    d <- colMeans (sqrt ((emb_mat - embeddings)^2))
    d <- data.frame (pkg = names (d), d = unname (d))

    index <- order (d$d)
    d_n <- d [index [seq_len (n)], ]
    rownames (d_n) <- NULL

    return (d_n)
}
