test_that ("tree-sitter", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    path <- pkgsimil_test_skeleton ()
    roxygen2::roxygenise (path)

    tags <- pkgsimil_tag_fns (path)
    expect_s3_class (tags, "data.frame")
    expect_true (nrow (tags) > 0L)
    expect_identical (names (tags), c ("fn", "name", "start", "end", "file"))
    expect_true ("demo::test_fn" %in% tags$fn)
    expect_true ("base::message" %in% tags$name)
})
