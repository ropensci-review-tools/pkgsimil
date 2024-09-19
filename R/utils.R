opt_is_quiet <- function () {
    options ("rlib_message_verbosity") == "quiet"
}

#' Estimate whether input text string is code or English prose text.
#'
#' This is only approximate, and there are even software packages which can
#' give false negatives and be identified as prose (like rOpenSci's "geonames"
#' package).
#'
#' @param txt Single input text string
#' @return Logical value indicating whether or not `txt` was identified as
#' code.
#' @export
text_is_code <- function (txt) {
    stopifnot (length (txt) == 1L)
    requireNamespace ("tokenizers", quietly = TRUE)

    token_threshold <- 0.98

    n0 <- length (strsplit (txt, "[[:space:]]+") [[1]])
    nw <- tokenizers::count_words (txt)
    nw / n0 < token_threshold
}

pkgsimil_cache_path <- function () {

    cache_dir <- Sys.getenv ("PKGSIMIL_CACHE_DIR")

    if (cache_dir == "") {
        cache_dir <- fs::path_expand (fs::path (
            rappdirs::user_cache_dir (),
            "R",
            "pkgsimil"
        ))
        if (!fs::dir_exists (cache_dir)) {
            fs::dir_create (cache_dir, recurse = TRUE)
        }
    }

    return (cache_dir)
}

# Function to estimate the `token_threshold` above of 0.98, from running over
# all rOpenSci packages.
# get_threshold <- function (paths) {
#     txt <- vapply (paths, get_pkg_text, character (1L))
#     n0 <- vapply (txt, function (i) length (strsplit (i, "[[:space:]]+") [[1]]), integer (1L), USE.NAMES = FALSE)
#     n1 <- vapply (txt, tokenizers::count_words, integer (1L), USE.NAMES = FALSE)
#     tok2word1 <- n1 / n0
#
#     code <- vapply (paths, get_pkg_code, character (1L))
#     n2 <- vapply (code, function (i) length (strsplit (i, "[[:space:]]+") [[1]]), integer (1L), USE.NAMES = FALSE)
#     n3 <- vapply (code, tokenizers::count_words, integer (1L), USE.NAMES = FALSE)
#     tok2word2 <- n3 / n2
#
#     prop_correct <- function (threshold, tok2word1, tok2word2) {
#         n_correct <- length (which (tok2word1 > threshold)) + length (which (tok2word2 < threshold))
#         n <- 2 * length (tok2word1)
#         1 - n_correct / n
#     }
#     op <- optimize (prop_correct, c (0, 1), tok2word1, tok2word2)
#     op$minimum
# }
