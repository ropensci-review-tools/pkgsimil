test_that ("similar pkgs text input", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    input <- "A similar package"
    n <- 5L
    out <- with_mock_dir ("sim_pkgs_txt", {
        pkgsimil_similar_pkgs (input, embeddings, n = n)
    })
    expect_type (out, "character")
    expect_length (out, n)
    expect_true (all (out %in% colnames (embeddings$text)))
})

test_that ("similar pkgs package input", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    path <- pkgsimil_test_skeleton ()
    roxygen2::roxygenise (path)

    n <- 5L
    out <- with_mock_dir ("sim_pkgs_pkg", {
        pkgsimil_similar_pkgs (path, embeddings, n = n)
    })
    expect_type (out, "list")
    expect_length (out, 2L)
    expect_identical (names (out), c ("text", "code"))
    expect_false (identical (out$text, out$code))
    expect_true (all (vapply (out, class, character (1L)) == "character"))
    lens <- vapply (out, length, integer (1L))
    expect_true (all (lens == n))
    out_vec <- unname (unlist (out))
    expect_true (all (out_vec %in% colnames (embeddings$text)))
})
