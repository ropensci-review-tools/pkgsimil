test_that ("similar pkgs text input", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    input <- "A similar package"
    n <- 5L
    embeddings <- get_test_embeddings (npkgs = 10, nfns = 10, embedding_len = 768)
    txt <- c ("a not so very similar package", "a test package", "a function to test")
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_txt", {
        pkgsimil_similar_pkgs (input, embeddings = embeddings, idfs = idfs, n = n)
    })
    expect_type (out, "character")
    expect_length (out, 2L)
    expect_true (all (out %in% colnames (embeddings$text_with_fns)))
})

test_that ("similar pkgs package input", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    path <- pkgsimil_test_skeleton ()
    roxygen2::roxygenise (path)

    n <- 5L
    embeddings <- get_test_embeddings (npkgs = 10, nfns = 10, embedding_len = 768)
    txt <- c ("a not so very similar package", "a test package", "a function to test")
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_pkg", {
        pkgsimil_similar_pkgs (path, embeddings = embeddings, idfs = idfs, n = n)
    })
    expect_type (out, "list")
    expect_length (out, 2L)
    expect_identical (names (out), c ("text", "code"))
    expect_false (identical (out$text, out$code))
    expect_true (all (vapply (out, class, character (1L)) == "character"))
    lens <- vapply (out, length, integer (1L))
    expect_true (all (lens == n))
    out_vec <- unname (unlist (out))
    expect_true (all (out_vec %in% colnames (embeddings$text_with_fns)))
})

test_that ("similar fns", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    embeddings_fns <- get_test_embeddings_fns (nfns = 10, embedding_len = 768)

    input <- "A test function"
    n <- 5L
    out <- with_mock_dir ("sim_fns", {
        pkgsimil_similar_fns (input, embeddings_fns, n = n)
    })
    expect_type (out, "character")
    expect_length (out, n)
    expect_true (all (out %in% colnames (embeddings_fns)))
})
