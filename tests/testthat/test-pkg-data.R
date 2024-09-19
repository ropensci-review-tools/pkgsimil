test_that ("get pkg local text", {
    path <- pkgsimil_test_skeleton ()
    expect_true (dir.exists (path))

    roxygen2::roxygenise (path) # Generate man files

    txt <- get_pkg_text (path)
    expect_type (txt, "character")
    expect_length (txt, 1L)
    expect_true (grepl ("##\\s*Functions", txt))
    expect_true (grepl ("#\\s*demo", txt))
    expect_true (nchar (txt) < 1000) # small test package

    code <- get_pkg_code (path)
    expect_type (code, "character")
    expect_length (code, 1L)
    expect_false (grepl ("#\\s*demo", code))
    expect_false (grepl ("##\\s*Functions", code))
    expect_true (grepl ("This function does nothing", code, fixed = TRUE))
    expect_true (nchar (code) < 1000)

    # ---- test utils fns -----
    skip_if_not (identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))
    # Identified as code because of markdown
    expect_true (text_is_code (txt))
    txt <- gsub ("\\n|#+", "", txt)
    expect_false (text_is_code (txt))
    expect_true (text_is_code (code))
})

test_that ("get pkg installed text", {
    pkg <- "cli"

    txt <- get_pkg_text (pkg)
    expect_type (txt, "character")
    expect_length (txt, 1L)
    expect_true (nchar (txt) > 1000)

    code <- get_pkg_code (pkg)
    expect_type (code, "character")
    expect_length (code, 1L)
    expect_true (nchar (txt) > 1000)

    code_exp_only <- get_pkg_code (pkg, exported_only = TRUE)
    expect_true (nchar (code_exp_only) / nchar (code) < 0.75)
})
