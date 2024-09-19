test_that ("similar pkgs", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    input <- "A similar package"
    n <- 5L
    out <- with_mock_dir ("sim_pkgs", {
        pkgsimil_similar_pkgs (input, embeddings, n = n)
    })
    expect_type (out, "character")
    expect_length (out, n)
    expect_true (all (out %in% colnames (embeddings$text)))
})
