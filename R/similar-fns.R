#' Identify functions best matching a given input string. Only applies to
#' functions from the corpus of rOpenSci packages.
#'
#' @inheritParams pkgsimil_similar_pkgs
#' @param input A text string.
#' @param embeddings A single matrix of embeddings produced from
#' \link{pkgmatch_embeddings_from_pkgs} with `functions_only = TRUE`. If not
#' cache directory.
#' provided, pre-generated embeddings will be downloaded and stored in a local
#' @return A character vector of function names in the form
#' "<package>::<function>".
#'
#' @family main
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Process raster satellite images"
#' pkgsimil_similar_fns (input)
#' }
pkgsimil_similar_fns <- function (input, embeddings = NULL, n = 5L) {

    if (is.null (embeddings)) {
        embeddings <- pkgsimil_load_data ("embeddings", corpus = "ropensci", fns = TRUE)
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
