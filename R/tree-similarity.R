#' Tree similarity
#'
#' @param trees Input trees as returned from \link{tree_parse} function, either
#' as list or vector of character objects with each element holding one tree.
#' @param num_cores Number of machine cores to use in parallel, defaulting to
#' single-core processing.
#' @return Similarity metrics
#' @export
tree_similarity <- function (trees, num_cores = 1L) {
    stopifnot (is.numeric (num_cores))
    num_cores <- as.integer (num_cores)
    stopifnot (length (num_cores) == 1L)
    if (is.list (trees)) {
        trees <- do.call (c, trees)
    }
    stopifnot (length (trees) > 1L)
    combs <- utils::combn (seq_along (trees), m = 2L)

    if (num_cores > 1L) {

        combs <- as.list (data.frame (combs))
        res <- parallel::mclapply (combs, function (i) {
            these_trees <- c (trees [i [1]], trees [i [2]])
            cpp_tree_similarity (these_trees)
        }, mc.cores = num_cores)

    } else {
        res <- pbapply::pbapply (combs, 2, function (i) {
            these_trees <- c (trees [i [1]], trees [i [2]])
            cpp_tree_similarity (these_trees)
        })
    }

    return (res)
}
