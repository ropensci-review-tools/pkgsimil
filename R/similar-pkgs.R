#' For an input of either a text string or local path to an R package, identify
#' most similar R packages from a specified corpus.
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
#'
#' @family main
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

        res <- similar_pkgs_from_pkg (input, embeddings)
        # Then combine BM25 from function calls with "code" similarities:
        bm25 <- pkgsimil_bm25_fn_calls (input, corpus = corpus)
        code_sim <- dplyr::left_join (res$code, bm25, by = "package")
        res$code <- pkgsimil_rerank (code_sim)
        res$text <- res$text$package

    } else {

        res <- similar_pkgs_from_text (
            input = input,
            embeddings = embeddings,
            idfs = idfs,
            input_is_code = input_is_code
        )
    }

    return (res)
}

similar_pkgs_from_pkg <- function (input, embeddings) {

    op <- options ()
    options (rlib_message_verbosity = "quiet")

    emb <- pkgsimil_embeddings_from_pkgs (input)

    options (op)

    npkgs <- ncol (embeddings$text_with_fns)
    nrow <- nrow (emb$text_with_fns)
    emb_text <- matrix (emb$text_with_fns, nrow = nrow, ncol = npkgs)
    d_text <- colSums (sqrt ((emb_text - embeddings$text_with_fns)^2))
    d_text <- data.frame (package = names (d_text), text = unname (d_text))

    npkgs <- ncol (embeddings$code)
    emb_code <- matrix (emb$code, nrow = nrow, ncol = npkgs)
    d_code <- colSums (sqrt ((emb_code - embeddings$code)^2))
    d_code <- data.frame (package = names (d_code), code = unname (d_code))

    out <- dplyr::left_join (d_text, d_code, by = "package")
    out$code <- out$code / max (out$code, na.rm = TRUE)
    out$text <- out$text / max (out$text, na.rm = TRUE)

    list (
        text = order_output (out, "text"),
        code = order_output (out, "code")
    )
}

order_output <- function (out, what = "text") {

    index <- order (out [[what]])
    out <- out [index, c ("package", what)]
    rownames (out) <- NULL

    return (out)
}

similar_pkgs_from_text <- function (input,
                                    embeddings = NULL,
                                    idfs = NULL,
                                    input_is_code = text_is_code (input)) {

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

    rm_fn_data <- !input_mentions_functions (input)

    return (pkgsimil_rerank (similarities, rm_fn_data))
}

input_mentions_functions <- function (input) {

    stopifnot (length (input) == 1L)

    grepl ("\\sfunction\\s", input, ignore.case = TRUE)
}
