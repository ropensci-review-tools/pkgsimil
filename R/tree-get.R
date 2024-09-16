#' Get a syntax tree for a specified package, including all functions both
#' exported and internal.
#'
#'
#'
#' @param pkg_name Name of one or more packages for which trees are to be extracted.
#' @param exported_only If `TRUE`, build trees only from exported functions;
#' otherwise use all functions.
#' @return A character vector of syntax trees, one for each nominated package.
#' @export
#'
#' @examples
#' \dontrun{
#' trees <- tree_get (c ("utils", "tools"))
#' # These are huge syntax trees:
#' sapply (trees, function (i) format (nchar (i), big.mark = ","))
#' }
tree_get <- function (pkg_name = NULL, exported_only = FALSE) {
    stopifnot (is.character (pkg_name))
    stopifnot (length (pkg_name) > 0L)
    stopifnot (is.logical (exported_only))

    namespace_okay <- vapply (pkg_name, function (i) {
        !is.null (tryCatch (asNamespace (i), error = function (e) NULL))
    }, logical (1L))
    if (any (!namespace_okay)) {
        pkg_name <- paste0 (pkg_name [which (!namespace_okay)])
        stop ("pkg_name [", pkg_name, "] is not a package, or is not installed.")
    }

    vapply (pkg_name, function (i) get_one_tree (i, exported_only), character (1L))
}

get_fn_defs <- function (pkg_name, exported_only) {
    # `lsf.str` is for functions only:
    fn_names <- unclass (
        utils::lsf.str (envir = asNamespace (pkg_name), all = TRUE)
    )
    if (exported_only) {
        suppressPackageStartupMessages (
            require (pkg_name, character.only = TRUE)
        )
        fns_exp <- ls (paste0 ("package:", pkg_name))
        fn_names <- fn_names [which (fn_names %in% fns_exp)]
    }
    fn_defs <- lapply (fn_names, function (f) {
        utils::getFromNamespace (f, pkg_name)
    })
    names (fn_defs) <- fn_names

    return (fn_defs)
}

get_one_tree <- function (pkg_name, exported_only) {

    fn_defs <- get_fn_defs (pkg_name, exported_only)

    trees <- vapply (fn_defs, function (f) {
        f |>
            deparse (width.cutoff = 500L) |>
            paste0 (collapse = "\n") |>
            tree_parse ()
    }, character (1L))
    paste0 ("(", paste0 (trees, collapse = " "), ")")
}
