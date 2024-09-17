get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools")

get_pkg_fns_text <- function (pkg_name = NULL, exported_only = FALSE) {

    stopifnot (length (pkg_name) == 1L)

    if (pkg_is_installed (pkg_name)) {
        fns <- get_fn_defs_namespace (pkg_name, exported_only = exported_only)
        fns <- vapply (seq_along (fns), function (i) {
            fi <- fns [[i]] |>
                deparse (width.cutoff = 500L) |>
                paste0 (collapse = "\n")
            paste0 (names (fns) [i], " <- ", fi)
        }, character (1L))

        paste0 (fns, collapse = "\n")
    } else {
        fns <- get_fn_defs_local (pkg_name)
    }

    return (fns)
}

pkg_is_installed <- function (pkg_name) {
    ip <- data.frame (installed.packages ())
    pkg_name %in% ip$Package
}

get_fn_defs_namespace <- function (pkg_name, exported_only) {
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

get_fn_defs_local <- function (path) {
    path <- fs::path_norm (path)
    stopifnot (fs::dir_exists (path))
    path_r <- fs::path (path, "R")
    stopifnot (fs::dir_exists (path_r))

    files_r <- fs::dir_ls (path_r, regexp = "\\.(r|R)$")
    txt <- lapply (files_r, brio::read_lines)
    txt <- unname (do.call (c, txt))
    index <- grep ("^[[:space:]]*#", txt)
    if (length (index) > 0L) {
        txt <- txt [-index]
    }
    txt <- txt [which (nzchar (txt))]
    txt <- gsub ("^[[:space:]]*", "", txt)
    paste0 (txt, collapse = "\n ")
}

get_pkg_text <- function (pkg_name) {
    if (pkg_is_installed (pkg_name)) {
        txt <- get_pkg_text_namespace (pkg_name)
    } else {
        txt <- get_pkg_text_local (pkg_name)
    }

    return (txt)
}

get_pkg_text_namespace <- function (pkg_name) {

    stopifnot (length (pkg_name) == 1L)

    desc <- utils::packageDescription (pkg = pkg_name, fields = "Description")
    desc <- gsub ("\\n", " ", desc)
    desc <- gsub ("\\s+", " ", desc)

    fns <- get_fn_descs (pkg_name)
    fns <- lapply (seq_len (nrow (fns)), function (i) {
        c (
            paste0 ("### ", gsub ("\\.Rd$", "", fns$rd_name [i])),
            "",
            fns$desc [i],
            ""
        )
    })

    paste0 (c (desc_template (pkg_name), unlist (fns)), collapse = "\n ")
}

desc_template <- function (pkg_name) {
    c (
        paste0 ("# ", pkg_name, "\n"),
        "",
        "## Description",
        "",
        desc,
        "",
        "## Functions",
        ""
    )
}

get_pkg_text_local <- function (path) {
    stopifnot (length (path) == 1L)

    path <- fs::path_norm (path)
    stopifnot (fs::dir_exists (path))

    desc_file <- fs::path (path, "DESCRIPTION")
    stopifnot (fs::file_exists (desc_file))
    desc <- data.frame (read.dcf (desc_file))$Description

    rd_path <- fs::path (path, "man")
    stopifnot (fs::file_exists (rd_path))
    rd_files <- fs::dir_ls (rd_path, regex = "\\.Rd")
    rd <- lapply (rd_files, function (i) {
        rd <- tools::parse_Rd (i)
        tags <- vapply (rd, function (j) attr (j, "Rd_tag"), character (1L))
        index <- which (tags == "\\description")
        if (length (index) == 0) return ("")
        rd_desc <- gsub ("\\n$", "", unlist (rd [[index]]))
        paste (rd_desc, collapse = "")
    })

    fns <- gsub ("\\.Rd$", "", basename (names (rd)))
    rd <- unname (unlist (rd))
    fn_txt <- lapply (seq_len (length (rd)), function (i) {
        c (
           paste0 ("### ", fns [i]),
           "",
           rd [i],
           ""
        )
    })

    out <- c (
        desc_template (basename (path)),
        unlist (fn_txt)
    )

    paste0 (out, collapse = "\n ")
}

get_fn_descs <- function (pkg_name) {

    rd <- tools::Rd_db (package = pkg_name)
    descs <- vapply (rd, function (i) {
        d <- get_Rd_metadata (i, "description")
    }, character (1L))
    descs <- gsub ("\\\\n", " ", descs)
    descs <- gsub ("\\n", " ", descs)
    descs <- gsub ("\\", "", descs, fixed = TRUE)
    descs <- gsub ("\\", "", descs, fixed = TRUE)
    descs <- gsub ("\\s+", " ", descs)

    index <- which (!is.na (descs))
    data.frame (
        desc = unname (descs),
        rd_name = names (descs)
    ) [index, ]
}
