test_that ("get pkg local text", {
    path <- pkgsimil_test_skeleton ()
    expect_true (dir.exists (path))

    roxygen2::roxygenise (path) # Generate man files

    txt <- get_pkg_text_local (path)
    expect_type (txt, "character")
    expect_length (txt, 1L)
    expect_true (grepl ("##\\s*Functions", txt))
    expect_true (grepl ("#\\s*demo", txt))

    code <- get_pkg_code (path)
    expect_type (code, "character")
    expect_length (code, 1L)
    expect_false (grepl ("#\\s*demo", code))
    expect_false (grepl ("##\\s*Functions", code))
    expect_true (grepl ("This function does nothing", code, fixed = TRUE))

    # ---- test utils fns -----
    # Identified as code because of markdown
    expect_true (text_is_code (txt))
    txt <- gsub ("\\n|#+", "", txt)
    expect_false (text_is_code (txt))
    expect_true (text_is_code (code))
})
