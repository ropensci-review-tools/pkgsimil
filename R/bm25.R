#' Calculate the "BM25" = "Best Matching 25" ranking function.
#'
#' See \url{https://en.wikipedia.org/wiki/Okapi_BM25}.
#'
#' @param txt A list of input documents.
#' @export
pkgsimil_bm25 <- function (doc, txt) {

    tokens_idf <- m_bm25_idf (txt)
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
#' @noRd
bm25_tokens_list <- function (txt) {

    tokens <- bm25_tokens (txt)

    m_bm25_tokens_list (tokens)
}

bm25_tokens_list_internal <- function (tokens) {

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
    tokens_txt <- bm25_tokens (txt)

    ntoks <- vapply (tokens_txt, length, integer (1L))
    ntoks_avg <- mean (ntoks [which (ntoks > 0L)])

    tokens_list <- bm25_tokens_list (txt)

    tokens_idf <- do.call (rbind, lapply (tokens_list, function (i) {
        data.frame (token = unique (i$token), n = 1L)
    })) |>
        dplyr::group_by (token) |>
        dplyr::summarise (n = dplyr::n ())
    tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
    tokens_idf$n <- NULL

    return (tokens_idf)
}

m_bm25_idf <- memoise::memoise (bm25_idf_internal)
