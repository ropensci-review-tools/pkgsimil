# Functions to attach namespaces to function calls, primarily from calls
# derived from treesitter output.

# base pkgs from R-devel/src/library:
base_pkgs <- c (
    "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk",
    "tools", "utils"
)

# Recommended from R-devel/src/library/Recommended:
rcmd_pkgs <- c (
    "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster",
    "codetools", "foreign", "lattice", "mgcv", "nlme", "nnet", "rpart",
    "spatial", "survival"
)

#' List all exported functions based on path to local package (either full
#' source repository, or extracted tarball).
#' @noRd
get_pkg_exported_fns <- function (path) {
    rd_files <- fs::dir_ls (fs::path (path, "man"), regexp = "\\.Rd$")
    fn_names <- lapply (rd_files, function (i) {
        suppressWarnings (
            rd <- tools::parse_Rd (i)
        )
        tags <- vapply (rd, function (j) {
            gsub ("^\\\\", "", attr (j, "Rd_tag"))
        }, character (1L))
        if (any (tags == "docType")) {
            docType <- as.character (rd [[which (tags == "docType")]] [[1]]) # nolint
            if (identical (docType, "package")) {
                return ("")
            }
        }

        index <- which (tags == "alias")
        if (length (index) == 0) {
            return ("")
        }
        gsub ("\\n$", "", unlist (rd [index]))
    })
    unlist (unname (fn_names))
}

#' Attach namespace of focal package to treesitter 'calls' output.
#' @noRd
attach_this_pkg_namespace <- function (calls) {

    fn_names <- get_pkg_exported_fns (path)

    index_in <- which (calls$fn %in% fn_names)
    index_out <- which (!calls$fn %in% fn_names)
    fns_exp <- calls$fn [index_in]
    fns_non_exp <- calls$fn [index_out]
    calls$fn [index_in] <- paste0 (pkg_name, "::", calls$fn [index_in])
    calls$fn [index_out] <- paste0 (pkg_name, ":::", calls$fn [index_out])

    name_in <- which (calls$name %in% fns_exp)
    name_out <- which (calls$name %in% fns_non_exp)
    calls$name [name_in] <- paste0 (pkg_name, "::", calls$name [name_in])
    calls$name [name_out] <- paste0 (pkg_name, ":::", calls$name [name_out])

    return (calls)
}

#' Attach namespaces of base and recommended packages to treesitter 'calls'
#' output.
#' @noRd
attach_base_rcmd_ns <- function (calls) {

    attach_ns <- function (calls, pkg_name) {
        index_no_ns <- which (!grepl ("\\:\\:", calls$name))
        suppressWarnings (
            base_fn_defs <- names (get_fn_defs_namespace (pkg_name, exported_only = TRUE))
        )
        index <- which (calls$name [index_no_ns] %in% base_fn_defs)
        calls$name [index_no_ns] [index] <- paste0 (
            pkg_name,
            "::",
            calls$name [index_no_ns] [index]
        )
        return (calls)
    }

    calls <- attach_ns (calls, "base")
    # From some reason, `.Call` is not listed in base:
    calls$name [calls$name == ".Call"] <- "base::.Call"

    for (p in base_pkgs) calls <- attach_ns (calls, p)
    for (p in rcmd_pkgs) calls <- attach_ns (calls, p)

    return (calls)
}
