#' treesim function'
#' @param node_brackets If `TRUE` (default), wrap all nodes in S-expression braces.
#' @export
treesim <- function (node_brackets = TRUE) {
    .Call ("c_parse_one_file", as.vector (node_brackets))
}
