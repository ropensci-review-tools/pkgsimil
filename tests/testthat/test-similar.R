test_that ("similar pkgs text input", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    input <- "A similar package"
    n <- 5L
    embeddings <- get_test_embeddings (
        npkgs = 10,
        nfns = 10,
        embedding_len = 768
    )
    txt <- c (
        "a not so very similar package",
        "a test package",
        "a function to test"
    )
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_txt", {
        pkgsimil_similar_pkgs (
            input,
            embeddings = embeddings,
            idfs = idfs,
            n = n
        )
    })
    expect_s3_class (out, "pkgmatch")
    expect_type (out, "list")
    expect_true (all (out$package %in% colnames (embeddings$text_with_fns)))
    expect_equal (attr (out, "n"), n)

    out_p <- strsplit (capture.output (print (out)), "\\\"\\s") [[1]]
    expect_length (out_p, n)
})

test_that ("similar pkgs package input", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    path <- pkgmatch_test_skeleton (pkg_name = "demo")
    roxygen2::roxygenise (path)

    n <- 5L
    npkgs <- 10L
    embeddings <- get_test_embeddings (
        npkgs = npkgs,
        nfns = npkgs,
        embedding_len = 768
    )
    txt <- c (
        "a not so very similar package",
        "a test package",
        "a function to test"
    )
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_pkg", {
        pkgsimil_similar_pkgs (
            path,
            embeddings = embeddings,
            idfs = idfs,
            n = n
        )
    })

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    expect_s3_class (out, "pkgmatch")
    expect_s3_class (out, "data.frame")
    expect_equal (attr (out, "n"), n)
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "text_rank", "code_rank"))
    expect_false (identical (out$text, out$code))
    expect_equal (nrow (out), npkgs)

    expect_true (all (out$package %in% colnames (embeddings$text_with_fns)))

    out_p <- capture.output (print (out))
    expect_true (any (grepl ("^\\$text$", out_p)))
    expect_true (any (grepl ("^\\$code$", out_p)))
    nm_lines <- grep ("^\\[", out_p, value = TRUE)
    lens <- vapply (
        nm_lines,
        function (n) length (strsplit (n, "\\\"\\s") [[1]]),
        integer (1L),
        USE.NAMES = FALSE
    )
    expect_true (all (lens == n))
})

test_that ("similar fns", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    nfns <- 10L
    embeddings_fns <- get_test_embeddings_fns (nfns = nfns, embedding_len = 768)

    input <- "A test function"
    n <- 5L
    out <- with_mock_dir ("sim_fns", {
        pkgsimil_similar_fns (input = input, embeddings = embeddings_fns, n = n)
    })
    expect_s3_class (out, "pkgmatch")
    expect_type (out, "list")
    expect_equal (nrow (out), nfns)
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "simil", "rank"))
    expect_true (all (out$package %in% colnames (embeddings_fns)))
    expect_identical (out$rank, seq_len (nrow (out)))

    out_p <- strsplit (capture.output (print (out)), "\\\"\\s") [[1]]
    expect_length (out_p, n)
})
