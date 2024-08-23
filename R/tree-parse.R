#' Parse syntax tree for one source code input (function, file, or other objects).
#'
#' @param source_code Single character element containing source code to be parsed.
#' @param node_brackets If `TRUE` (default), wrap all nodes in S-expression braces.
#' @return An S-expression representing the source code. Most elements of this
#' S-expression are generic and hold "field names" from tree-sitter. These are
#' defined for the R language in
#' \url{https://github.com/r-lib/tree-sitter-r/blob/main/src/grammar.json}, by
#' the 'name' element of every entry that has 'type: "field"'. Unique field
#' names are:
#' \itemize{
#' \item alternative
#' \item argument
#' \item arguments
#' \item body
#' \item close
#' \item condition
#' \item consequence
#' \item content
#' \item default
#' \item function
#' \item lhs
#' \item name
#' \item open
#' \item operator
#' \item operator
#' \item parameters
#' \item rhs
#' \item sequence
#' \item value
#' \item variable
#' }
#' The `pkgsimil` parser produces standard tree-sitter output except for the
#' "name" and "function" fields, which are replaced by the actual strings
#' naming those objects. This enables trees to be compared no just in terms
#' both of similarity of structures and calls or references to named objects
#' such as functions.
#' @export
#'
#' @examples
#' code <- simpleError |> # Note no `()` to get and not call function.
#'     deparse (width.cutoff = 500L) |>
#'     paste0 (collapse = "\n")
#' tree_parse (code, TRUE)
tree_parse <- function (source_code = NULL, node_brackets = TRUE) {
    .Call (c_parse_one_file, as.vector (source_code), as.vector (node_brackets))
}
