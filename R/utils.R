opt_is_quiet <- function () {
    options ("rlib_message_verbosity") == "quiet"
}

#' Estimate whether input text string is code or English prose text.
#'
#' This is only approximate, and there are even software packages which can
#' give false negatives and be identified as prose (like rOpenSci's "geonames"
#' package), and prose which may be wrongly identified as code.
#'
#' @param txt Single input text string
#' @return Logical value indicating whether or not `txt` was identified as
#' code.
#' @export
#'
#' @examples
#' txt <- "Some text without any code"
#' text_is_code (txt)
#' txt <- "this_is_code <- function (x) { x }"
#' text_is_code (txt)
text_is_code <- function (txt) {
    stopifnot (length (txt) == 1L)

    token_threshold <- 0.98

    n0 <- length (strsplit (txt, "[[:space:]]+") [[1]])
    nw <- tokenizers::count_words (txt)
    nw / n0 < token_threshold
}

#' Check whether 'input' parameter is a directory or not.
#'
#' This is necessary because `fs::dir_exists()` errors if the string passed is
#' too long.
#' @noRd
input_is_dir <- function (input) {

    chk <- tryCatch (
        fs::dir_exists (input),
        error = function (e) NULL
    )
    ifelse (is.null (chk), FALSE, chk)
}

#' Embeddings functions only return columns for input items that have > 0
#' characters. This reduces `nms` to the appropriate length before applying as
#' column names.
#' @param obj Object for which column names are to be added.
#' @param src Source of column names, generally a named character vector.
#' @param nms Vector of names to be applied.
#' @noRd
apply_col_names <- function (obj, src, nms) {
    index <- which (nzchar (src))
    colnames (obj) <- nms [index]

    return (obj)
}

# Function to estimate the `token_threshold` above of 0.98, from running over
# all rOpenSci packages.
# get_threshold <- function (paths) {
#     txt <- vapply (paths, get_pkg_text, character (1L))
#     n0 <- vapply (
#         txt,
#         function (i) {
#             length (strsplit (i, "[[:space:]]+") [[1]])
#         },
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     n1 <- vapply (
#         txt,
#         tokenizers::count_words,
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     tok2word1 <- n1 / n0
#
#     code <- vapply (paths, get_pkg_code, character (1L))
#     n2 <- vapply (
#         code,
#         function (i) length (strsplit (i, "[[:space:]]+") [[1]]),
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     n3 <- vapply (
#         code,
#         tokenizers::count_words,
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     tok2word2 <- n3 / n2
#
#     prop_correct <- function (threshold, tok2word1, tok2word2) {
#         n_correct <- length (which (tok2word1 > threshold)) +
#             length (which (tok2word2 < threshold))
#         n <- 2 * length (tok2word1)
#         1 - n_correct / n
#     }
#     op <- optimize (prop_correct, c (0, 1), tok2word1, tok2word2)
#     op$minimum
# }
