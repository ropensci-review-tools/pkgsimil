opt_is_quiet <- function () {
    options ("rlib_message_verbosity") == "quiet"
}

#' Estimate whether input text string is code or English prose text.
#'
#' This is only approximate, and there are even software packages which can
#' give false negatives and be identified as prose (like rOpenSci's "geonames"
#' package). Empirical tests nevertheless suggest that the function has an
#' accuracy above 95%.
#'
#' @param txt Single input text string
#' @return Logical value indicating whether or not `txt` was identified as
#' code.
#' @export
text_is_code <- function (txt) {
    stopifnot (length (txt) == 1L)
    requireNamespace ("tokenizers")

    token_threshold <- 0.9

    n0 <- length (strsplit (txt, "[[:space:]]") [[1]])
    nw <- tokenizers::count_words (txt)
    nw / n0 < token_threshold
}
