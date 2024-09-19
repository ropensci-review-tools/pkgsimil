# From srr/R/pkg-skeleon.R

make_pkg_path <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- file.path (base_dir, pkg_name)
    if (!file.exists (d)) {
        dir.create (d, recursive = TRUE)
    }

    return (d)
}

write_desc <- function (d, pkg_name) {

    desc <- c (
        paste0 ("Package: ", pkg_name),
        "Title: What the Package Does (One Line, Title Case)",
        "Version: 0.0.0.9000",
        "Authors@R: ",
        "  person(given = \"First\",",
        "         family = \"Last\",",
        "         role = c(\"aut\", \"cre\"),",
        "         email = \"first.last@example.com\")",
        "Description: What the package does (one paragraph).",
        "Imports:",
        "    Rcpp",
        "Suggests:",
        "    testthat",
        "LinkingTo:",
        "    Rcpp",
        "License: GPL-3",
        "Encoding: UTF-8"
    )

    writeLines (desc, con = file.path (d, "DESCRIPTION"))
}

write_r_fn <- function (d, pkg_name) {

    rfile <- c (
        "#' test_fn",
        "#'",
        "#' A test funtion",
        "#'",
        "#' @export",
        "test_fn <- function() {",
        "  message(\"This function does nothing\")",
        "}"
    )
    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }
    writeLines (rfile, con = file.path (dr, "test.R"))

    rfile <- c (
        "#' @keywords internal",
        "\"_PACKAGE\"",
        "",
        paste0 (
            "# The following block is used by ",
            "usethis to automatically manage"
        ),
        "# roxygen namespace tags. Modify with care!",
        "## usethis namespace: start",
        "## usethis namespace: end",
        "NULL"
    )
    writeLines (rfile, con = file.path (dr, paste0 (pkg_name, "-package.R")))
}

write_readme <- function (d, pkg_name) {

    # nolint start --- lines > 80 characters
    rfile <- c (
        paste0 ("# ", pkg_name),
        "",
        "This `README.Rmd` file is here to demonstrate some stuff.",
        "",
        "``` r",
        "  x <- 1",
        "```",
        "",
        "The end."
    )
    # nolint end

    writeLines (rfile, con = file.path (d, "README.md"))
}


pkgsimil_test_skeleton <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- make_pkg_path (base_dir, pkg_name)

    if (length (list.files (d)) > 0L) {
        stop (
            "The path [", d, "] is not empty; ",
            "can only make a package in an empty directory\n",
            "  Directory can be cleared with ",
            "'unlink(<dir>, recursive = TRUE)'"
        )
    }
    write_desc (d, pkg_name)
    write_r_fn (d, pkg_name)
    write_readme (d, pkg_name)

    return (d)
}
