# Get all R function calls from tree-sitter
#
# Adapated from code provided by Davis Vaughan in
# https://github.com/ropensci-review-tools/pkgstats/pull/62#issuecomment-2359834446

QUERY_FUNCTIONS <- r"(
  (binary_operator
      lhs: (identifier) @name
      operator: "<-"
      rhs: (function_definition) @fn
  )
)"
QUERY_FUNCTIONS <- treesitter::query (treesitter.r::language (), QUERY_FUNCTIONS)

QUERY_FUNCTIONS_EQ <- r"(
  (binary_operator
      lhs: (identifier) @name
      operator: "="
      rhs: (function_definition) @fn
  )
)"
QUERY_FUNCTIONS_EQ <- treesitter::query (treesitter.r::language (), QUERY_FUNCTIONS_EQ)

QUERY_CALLS <- r"(
  (call
    function: [
      (identifier) @name
      (namespace_operator) @name
    ]
  )
)"
QUERY_CALLS <- treesitter::query (treesitter.r::language (), QUERY_CALLS)

#' Extract names of all function called from a given treesitter node
#'
#' This first identifies all functions, then recursively identifies all calls
#' within the bodies of those functions (by calling the `get_calls()`
#' function).
#' @noRd
get_calls_in_functions <- function (node) {
    functions <- treesitter::query_captures (QUERY_FUNCTIONS, node)
    names <- functions$node [functions$name == "name"]
    bodies <- functions$node [functions$name == "fn"]

    functions_eq <- treesitter::query_captures (QUERY_FUNCTIONS_EQ, node)
    names <- c (names, functions_eq$node [functions_eq$name == "name"])
    bodies <- c (bodies, functions_eq$node [functions_eq$name == "fn"])

    tibble::new_tibble (list (
        fn = vapply (names, treesitter::node_text, character (1)),
        info = lapply (bodies, get_calls)
    ))
}

#' Extract all function calls from a given treesitter node.
#'
#' This is called from the preceding `get_calls_in_functions()`, and is applied
#' to each individual function body, to identify all calls make within that
#' function.
#' @noRd
get_calls <- function (node) {
    captures <- treesitter::query_captures (QUERY_CALLS, node)
    name <- vapply (
        captures$node,
        function (node) treesitter::node_text (node),
        character (1)
    )
    start <- vapply (
        captures$node,
        function (node) {
            treesitter::point_row (treesitter::node_start_point (node))
        },
        double (1)
    )
    end <- vapply (
        captures$node,
        function (node) {
            treesitter::point_row (treesitter::node_end_point (node))
        },
        double (1)
    )

    tibble::new_tibble (list (
        name = name,
        start = start,
        end = end
    ))
}

#' The main function applied to a package `path`, and used to identify and
#' return all function calls made within the package.
#'
#' @noRd
tressitter_calls_in_package <- function (path) {

    # supreess 'no visible binding' notes:
    info <- NULL

    path_r <- fs::path (path, "R")
    paths <- fs::dir_ls (path_r)
    paths <- as.character (paths)

    parser <- treesitter::parser (treesitter.r::language ())

    out <- vector ("list", length = length (paths))

    for (i in seq_along (out)) {
        path <- paths [[i]]
        text <- brio::read_file (path)
        tree <- treesitter::parser_parse (parser, text)
        node <- treesitter::tree_root_node (tree)
        elt <- get_calls_in_functions (node)
        elt [["file"]] <- as.character (path)
        out [[i]] <- elt
    }

    out <- vctrs::list_unchop (out)
    out <- tidyr::unnest (out, info)
    out
}

#' Use "treesitter" to tag all function calls made within local package, and to
#' associate those calls with package namespaces.
#'
#' @param path Path to local package, or `.tar.gz` file of package source.
#' @return A `data.frame` of all function calls made within the package, with
#' the following columns:
#' \itemize{
#' \item 'fn' Name of the package function within which call is made, including
#' namespace identifiers of "::" for exported functions and ":::" for
#' non-exported functions.
#' \item name Name of function being called, including namespace.
#' \item start Byte number within file corresponding to start of definition
#' \item end Byte number within file corresponding to end of definition
#' \item file Name of file in which fn call is defined.
#' }
#'
#' @export
pkgsimil_tag_fns <- function (path) {

    calls <- tressitter_calls_in_package (path)
    calls <- attach_this_pkg_namespace (path, calls)
    calls <- attach_base_rcmd_ns (calls)
    calls <- attach_local_dep_namespaces (path, calls)

    return (calls)
}
