test_that ("embeddings properties", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    txt <- "test text"
    emb <- with_mock_dir ("emb_test_text", {
        get_embeddings (txt)
    })
    expect_type (emb, "double")
    expect_length (dim (emb), 2L)
    expect_equal (nrow (emb), expected_embedding_length) # in helper-embeddings.R
    expect_true (min (emb) < 0)
    expect_true (max (emb) > 0)
})

test_that ("embedding_dists fn", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    packages <- c ("cli", "fs")
    ncombs <- ncol (combn (packages, 2L))
    d <- with_mock_dir ("emb_pkgs", {
        pkgsimil_embedding_dists (packages)
    })
    expect_s3_class (d, "data.frame")
    expect_equal (ncol (d), 4L)
    expect_equal (nrow (d), ncombs)
    expect_identical (names (d), c ("from", "to", "d_text", "d_code"))
    expect_type (d$d_text, "double")
    expect_type (d$d_code, "double")
    expect_true (all (packages %in% c (d$from, d$to)))
})

test_that ("raw embeddings", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    packages <- "cli"
    emb <- with_mock_dir ("emb_raw", {
        pkgsimil_embeddings_raw (packages)
    })
    expect_type (emb, "list")
    expect_identical (names (emb), c ("text", "code"))
    is_mat <- vapply (emb, function (i) length (dim (i)) == 2L, logical (1L))
    expect_true (all (is_mat))
    ncol <- vapply (emb, ncol, integer (1L))
    expect_true (all (ncol == 1L))
    nrow <- vapply (emb, nrow, integer (1L))
    expect_true (all (nrow == expected_embedding_length))
})
