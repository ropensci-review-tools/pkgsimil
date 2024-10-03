#' Use the embeddings from \link{pkgsimil_embeddings_from_pkgs} to identify
#' most similar packages to a given local repository.
#'
#' @param input Either a path to local source code of an R package, or a text
#' string.
#' @param corpus If `embeddings` or `idfs` parameters are not specified, they
#' are automatically downloaded for the corpus specified by this parameter.
#' Must be one of "ropensci" or "cran". The function will then return the most
#' similar package from the specified corpus.
#' @param embeddings Large Language Model embeddings for all rOpenSci packages,
#' generated from \link{pkgsimil_embeddings_from_pkgs}. If not provided,
#' pre-generated embeddings will be downloaded and stored in a local cache
#' directory.
#' @param idfs Inverse Document Frequency tables for all rOpenSci packages,
#' generated from \link{pkgsimil_bm25}. If not provided, pre-generated IDF
#' tables will be downloaded and stored in a local cache directory.
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
#' @note The first time this function is run without passing either
#' `embeddings` or `idfs`, required values will be automatically downloaded and
#' stored in a locally persistent cache directory. Especially for the "cran"
#' corpus, this downloading may take quite some time.
#'
#' @seealso input_is_code
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Download open spatial data from NASA"
#' pkgsimil_similar_pkgs (input)
#' }
pkgsimil_similar_pkgs <- function (input,
                                   corpus = "ropensci",
                                   embeddings = NULL,
                                   idfs = NULL,
                                   input_is_code = text_is_code (input),
                                   n = 5L) {

    corpus <- match.arg (corpus, c ("ropensci", "cran"))

    if (is.null (embeddings)) {
        embeddings <- pkgsimil_load_data (what = "embeddings", corpus = corpus)
    }
    if (is.null (idfs)) {
        idfs <- pkgsimil_load_data (what = "idfs", corpus = corpus)
    }

    nms_expected <- c ("text_with_fns", "text_wo_fns", "code")
    stopifnot (is.list (embeddings))
    stopifnot (identical (names (embeddings), nms_expected))
    stopifnot (is.list (idfs))
    stopifnot (identical (names (idfs), c ("idfs", "token_lists")))

    if (input_is_dir (input)) {

        res <- similar_pkgs_from_pkg (input, embeddings, n)
        res <- lapply (res, function (i) i$package)

    } else {

        res <- similar_pkgs_from_text (
            input = input,
            embeddings = embeddings,
            idfs = idfs,
            input_is_code = input_is_code,
            n = n
        )
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
    d_text <- data.frame (package = names (d_text), text = unname (d_text))
    d_code <- colSums (sqrt ((emb_code - embeddings$code)^2))
    d_code <- data.frame (package = names (d_code), code = unname (d_code))

    out <- dplyr::left_join (d_text, d_code, by = "package")
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Process raster satellite images"
#' pkgsimil_similar_fns (input)
#' }
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

    n <- min (c (n, nrow (out)))
    index <- order (out [[what]])
    out <- out [index [seq_len (n)], c ("package", what)]
    rownames (out) <- NULL

    return (out)
}

similar_pkgs_from_text <- function (input,
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
        similarities <- similarity_embeddings (
            input,
            embeddings$code,
            input_is_code = TRUE
        )
    } else {
        similarities <- similarity_embeddings (
            input,
            embeddings,
            input_is_code = FALSE
        )
    }

    similarities_bm25 <- pkgsimil_bm25 (input = input, idfs = idfs)

    similarities <- dplyr::left_join (
        similarities,
        similarities_bm25,
        by = "package"
    )
    similarities [is.na (similarities)] <- 0

    index <- seq_len (n)
    rm_fn_data <- !input_mentions_functions (input)

    return (pkgsimil_rerank (similarities, rm_fn_data) [index])
}

input_mentions_functions <- function (input) {

    stopifnot (length (input) == 1L)

    grepl ("\\sfunction\\s", input, ignore.case = TRUE)
}
