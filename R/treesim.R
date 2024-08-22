#' treesim function'
#' @param source_code Single character element containing source code to be parsed.
#' @param node_brackets If `TRUE` (default), wrap all nodes in S-expression braces.
#' @export
treesim <- function (source_code = NULL, node_brackets = TRUE) {
    .Call ("c_parse_one_file", as.vector (source_code), as.vector (node_brackets))
}
