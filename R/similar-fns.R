#' Identify R functions best matching a given input string
#'
#' @description Function matching is only available for Only applies to
#' functions from the corpus of rOpenSci packages.
#'
#' @inheritParams pkgmatch_similar_pkgs
#' @param input A text string.
#' @return A character vector of function names in the form
#' "<package>::<function>".
#'
#' @family main
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Process raster satellite images"
#' p <- pkgmatch_similar_fns (input)
#' p # Default print method, lists 5 best matching packages
#' head (p) # Shows first 5 rows of full `data.frame` object
#' }
pkgmatch_similar_fns <- function (input, embeddings = NULL, n = 5L) {

    if (is.null (embeddings)) {
        embeddings <- pkgmatch_load_data ("embeddings", corpus = "ropensci", fns = TRUE)
    }
    stopifnot (is.matrix (embeddings))
    stopifnot (is.character (input))
    stopifnot (length (input) == 1L)

    op <- options ()
    options (rlib_message_verbosity = "quiet")
    emb <- get_embeddings (input)
    options (op)

    res <- cosine_similarity (emb [, 1], embeddings)
    res$rank <- seq_len (nrow (res))

    class (res) <- c ("pkgmatch", class (res))
    attr (res, "n") <- as.integer (n)

    return (res)
}
