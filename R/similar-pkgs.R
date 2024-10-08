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
#' @param When the result of this function is printed to screen, the top `n`
#' packages will be displayed.
#'
#' @return If `input` is a path to a local package, a list of two `data.frame`
#' objects with data quantifying similarities in terms of descriptive textual
#' similarity ("text"), and in terms of similarity of code structure ("code").
#'
#' vectors naming the `n` most similar packages in terms of descriptive textual
#' similarity ("text"), and in terms of similarity of code structure ("code").
#' If `input` is a single text string, a single `data.frame` object is returned
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
    nms_expected <- c ("text_with_fns", "text_wo_fns", "code")
    stopifnot (is.list (embeddings))
    stopifnot (identical (names (embeddings), nms_expected))

    if (is.null (idfs)) {
        idfs <- pkgsimil_load_data (what = "idfs", corpus = corpus)
    }
    stopifnot (is.list (idfs))
    stopifnot (identical (names (idfs), c ("idfs", "token_lists")))

    if (input_is_dir (input)) {

        res <- similar_pkgs_from_pkg (input, embeddings)
        # Then add BM25 from package text:
        txt_with_fns <- get_pkg_text (input)
        txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns) [[1]]
        bm25_with_fns <-
            pkgsimil_bm25 (txt_with_fns, idfs = idfs, corpus = corpus)
        bm25_wo_fns <- pkgsimil_bm25 (txt_wo_fns, idfs = idfs, corpus = corpus)
        # bm25 fn returns measures against idfs with and without fns:
        bm25_with_fns$bm25_wo_fns <- NULL
        bm25_wo_fns$bm25_with_fns <- NULL
        bm25_text <- dplyr::left_join (bm25_with_fns, bm25_wo_fns, by = "package")
        res <- dplyr::left_join (res, bm25_text, by = "package") |>
            dplyr::relocate (code, .after = dplyr::last_col ())

        # Then combine BM25 from function calls with "code" similarities:
        bm25_code <- pkgsimil_bm25_fn_calls (input, corpus = corpus) |>
            dplyr::rename (bm25_code = "bm25")

        res <- dplyr::left_join (res, bm25_code, by = "package")

        rm_fn_data <- TRUE # TODO: Expose that parameter

    } else {

        res <- similar_pkgs_from_text (
            input = input,
            embeddings = embeddings,
            idfs = idfs,
            corpus = corpus,
            input_is_code = input_is_code
        )

        rm_fn_data <- !input_mentions_functions (input)

    }

    res <- pkgsimil_rerank (res, rm_fn_data)

    class (res) <- c ("pkgsimil", class (res))
    attr (res, "n") <- as.integer (n)

    return (res)
}

similar_pkgs_from_pkg <- function (input, embeddings) {

    op <- options ()
    options (rlib_message_verbosity = "quiet")

    emb <- pkgsimil_embeddings_from_pkgs (input)

    options (op)

    d_text <- lapply (
        c ("text_with_fns", "text_wo_fns"),
        function (what) {
            npkgs <- ncol (embeddings [[what]])
            nrow <- nrow (emb [[what]])
            emb_text <- matrix (emb [[what]], nrow = nrow, ncol = npkgs)
            d_text <- colSums (sqrt ((emb_text - embeddings [[what]])^2))
            d_text <- data.frame (package = names (d_text), text = unname (d_text))
            names (d_text) [2] <- what
            return (d_text)
        }
    )
    d_text <- dplyr::left_join (d_text [[1]], d_text [[2]], by = "package")

    nrow <- nrow (embeddings$code)
    npkgs <- ncol (embeddings$code)
    emb_code <- matrix (emb$code, nrow = nrow, ncol = npkgs)
    d_code <- colSums (sqrt ((emb_code - embeddings$code)^2))
    d_code <- data.frame (package = names (d_code), code = unname (d_code))

    out <- dplyr::left_join (d_text, d_code, by = "package")
    out$code <- out$code / max (out$code, na.rm = TRUE)
    out$text_with_fns <- out$text_with_fns / max (out$text_with_fns, na.rm = TRUE)
    out$text_wo_fns <- out$text_wo_fns / max (out$text_wo_fns, na.rm = TRUE)

    return (out)
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
                                    corpus = "ropensci",
                                    input_is_code = text_is_code (input)) {

    stopifnot (is.character (input))
    stopifnot (length (input) == 1L)

    if (is.null (embeddings)) {
        embeddings <- pkgsimil_load_data (what = "embeddings", corpus = corpus)
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

    similarities_bm25 <-
        pkgsimil_bm25 (input = input, idfs = idfs, corpus = corpus)

    similarities <- dplyr::left_join (
        similarities,
        similarities_bm25,
        by = "package"
    )
    similarities [is.na (similarities)] <- 0

    return (similarities)
}

input_mentions_functions <- function (input) {

    stopifnot (length (input) == 1L)

    grepl ("\\sfunction\\s", input, ignore.case = TRUE)
}
