#' Calculate the "BM25" = "Best Matching 25" ranking function.
#'
#' See \url{https://en.wikipedia.org/wiki/Okapi_BM25}.
#'
#' @param input A single character string to match against the second parameter
#' of all input documents.
#' @param txt An optional list of input documents. If not specified, data will
#' be loaded as specified by the `corpus` parameter.
#' @param idfs Optional list of Inverse Document Frequency weightings generated
#' by the \link{bm25_idf} function. If not specified, values for the rOpenSci
#' corpus will be automatically downloaded and used.
#' @param corpus If `txt` is not specified, data for nominated corpus will be
#' downloaded to local cache directory, and BM25 values calculated against
#' those. Must be one of "ropensci", "ropensci-fns".
#'
#' @return A `data.frame` of package names and 'BM25' measures against text
#' from whole packages both with and without function descriptions.
#'
#' @export
pkgsimil_bm25 <- function (input, txt = NULL,
                           idfs = NULL, corpus = "ropensci") {

    if (is.null (txt)) {
        if (is.null (idfs)) {
            idfs <- pkgsimil_load_data ("idfs", fns = FALSE)
        }
        tokens_idf <- idfs$idfs
        tokens_list <- idfs$token_lists
    } else {
        stopifnot (is.list (txt))
        txt_lens <- vapply (txt, length, integer (1L))
        stopifnot (all (txt_lens == 1L))
        txt_class <- vapply (txt, class, character (1L))
        stopifnot (all (txt_class == "character"))

        tokens_list <- bm25_tokens_list (txt)
        tokens_idf <- bm25_idf (txt)
    }

    bm25_with_fns <- pkgsimil_bm25_from_idf (
        input,
        tokens_list$with_fns,
        tokens_idf$with_fns
    )
    bm25_wo_fns <- pkgsimil_bm25_from_idf (
        input,
        tokens_list$wo_fns,
        tokens_idf$wo_fns
    )
    names (bm25_with_fns) [2] <- "bm25_with_fns"
    names (bm25_wo_fns) [2] <- "bm25_wo_fns"

    dplyr::left_join (bm25_with_fns, bm25_wo_fns, by = "package")
}

pkgsimil_bm25_from_idf <- function (input, tokens_list, tokens_idf) {

    n <- NULL # suppress no visible binding note

    ntoks_list <- vapply (tokens_list, function (i) sum (i$n), integer (1L))
    ntoks_avg <- mean (ntoks_list)

    tokens_i <- bm25_tokens_list (input) [[1]]
    tokens_i <- dplyr::rename (tokens_i, np = n)

    # Fixed parameters used in the BM25 function. See wikipedia reference above
    # for these values.
    k <- 1.2
    b <- 0.75

    bm25 <- vapply (tokens_list, function (i) {
        len_i <- sum (i$n)
        toks_i <- dplyr::left_join (tokens_i, i, by = "token") |>
            dplyr::left_join (tokens_idf, by = "token") |>
            dplyr::filter (!is.na (n))
        numer <- toks_i$n * (k + 1)
        denom <- (toks_i$n + k * (1 - b + b * len_i / ntoks_avg))
        toks_i$score <- toks_i$idf * numer / denom
        sum (toks_i$score)
    }, numeric (1L))
    index <- order (bm25, decreasing = TRUE)
    bm25 <- data.frame (
        package = basename (names (bm25)),
        bm25 = unname (bm25)
    ) [index, ]
    rownames (bm25) <- NULL

    return (bm25)
}

#' Convert input list of text documents into lists of tokens.
#'
#' @inheritParams pkgsimil_bm25
#' @return The input list of text strings converted to tokens.
#' @noRd
bm25_tokens <- function (txt) {

    m_bm25_tokens (txt)
}

bm25_tokens_internal <- function (txt) {

    tokens <- tokenizers::tokenize_words (
        txt,
        lowercase = TRUE,
        strip_punct = TRUE,
        strip_numeric = TRUE
    )

    tokens <- lapply (tokens, function (i) {
        index <- grep ("\\.|\\_|^[0-9]", i)
        if (length (index) > 0) {
            i <- i [-(index)]
        }
        return (i)
    })

    return (tokens)
}

m_bm25_tokens <- memoise::memoise (bm25_tokens_internal)

#' Convert input list of raw tokens to a list of tokens and corresponding
#' frequencies.
#' @inheritParams pkgsimil_bm25
#' @return A list of `data.frame` objects, one for each input item, and each
#' including two columns of "token" and "n" holding frequencies for each token.
#' @export
bm25_tokens_list <- function (txt) {

    tokens <- bm25_tokens (txt)

    m_bm25_tokens_list (tokens)
}

bm25_tokens_list_internal <- function (tokens) {

    token <- NULL # suppress no visible binding note

    lapply (tokens, function (i) {
        data.frame (token = i) |>
            dplyr::group_by (token) |>
            dplyr::summarise (n = dplyr::n ())
    })
}

m_bm25_tokens_list <- memoise::memoise (bm25_tokens_list_internal)

#' Calculate inverse document frequencies for all tokens across a list of
#' documents.
#'
#' @inheritParams pkgsimil_bm25
#' @return A list of `data.frame` objects, each containing two columns of
#' "tokens" and "idf" for inverse document frequencies for each token.
#' @export
bm25_idf <- function (txt) {

    m_bm25_idf (txt)
}

bm25_idf_internal <- function (txt) {

    token <- NULL # suppress no visible binding note

    n_docs <- length (txt)

    tokens_txt <- bm25_tokens (txt)

    tokens_list <- bm25_tokens_list (txt)
    index <- which (vapply (tokens_list, nrow, integer (1L)) > 0L)

    tokens_idf <- do.call (rbind, lapply (tokens_list [index], function (i) {
        data.frame (token = unique (i$token), n = 1L)
    })) |>
        dplyr::group_by (token) |>
        dplyr::summarise (n = dplyr::n ())
    tokens_idf$idf <-
        log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
    tokens_idf$n <- NULL

    return (tokens_idf)
}

m_bm25_idf <- memoise::memoise (bm25_idf_internal)
