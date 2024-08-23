#' Get a syntax tree for a specified package, including all functions both
#' exported and internal.
#'
#'
#'
#' @param pkg_name Name of one or more packages for which trees are to be extracted.
#' @return A character vector of syntax trees, one for each nominated package.
#' @export
#'
#' @examples
#' \dontrun{
#' trees <- tree_get (c ("utils", "tools"))
#' # These are huge syntax trees:
#' sapply (trees, function (i) format (nchar (i), big.mark = ","))
#' }
tree_get <- function (pkg_name = NULL) {
    stopifnot (is.character (pkg_name))
    stopifnot (length (pkg_name) > 0L)

    vapply (pkg_name, get_pkg_tree, character (1L))
}

get_one_tree <- function (pkg_name) {
    # `lsf.str` is for functions only:
    fn_names <- unclass (utils::lsf.str (envir = asNamespace (pkg_name), all = TRUE))
    fn_defs <- lapply (fn_names, function (f) {
        utils::getFromNamespace (f, pkg_name)
    })
    names (fn_defs) <- fn_names

    trees <- vapply (fn_defs, function (f) {
        f |>
            deparse (width.cutoff = 500L) |>
            paste0 (collapse = "\n") |>
            tree_parse ()
    }, character (1L))
    paste0 ("(", paste0 (trees, collapse = " "), ")")
}
