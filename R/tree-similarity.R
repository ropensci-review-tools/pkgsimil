#' Tree similarity
#'
#' @param tree Input tree as S-expression.
#' @return Similarity metrics
#' @export
tree_similarity <- function (tree) {
    cpp_tree_similarity (tree)
}
