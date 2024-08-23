#' Tree similarity
#'
#' @param trees Input trees as returned from \link{tree_parse} function, either
#' as list or vector of character objects with each element holding one tree.
#' @return Similarity metrics
#' @export
tree_similarity <- function (trees) {
    if (is.list (trees)) {
        trees <- do.call (c, trees)
    }
    stopifnot (length (trees) > 1L)
    combs <- combn (seq_along (trees), m = 2L)

    apply (combs, 2, function (i) {
        these_trees <- c (trees [i [1]], trees [i [2]])
        cpp_tree_similarity (these_trees)
    })
}
