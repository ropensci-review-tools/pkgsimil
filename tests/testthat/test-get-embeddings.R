expect_embeddings_matrix <- function (x) {
    expect_type (x, "double")
    expect_length (dim (x), 2L)
    expect_equal (nrow (x), expected_embedding_length) # in helper-embeddings.R
    expect_true (min (x) < 0)
    expect_true (max (x) > 0)
}

test_that ("embeddings properties", {

    withr::local_envvar (list ("PKGSIMIL_TESTS" = "true"))

    txt <- "test text"
    emb <- with_mock_dir ("emb_test_text", {
        get_embeddings (txt)
    })
    expect_embeddings_matrix (emb)
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
    expect_embeddings_matrix (emb$text)
    expect_embeddings_matrix (emb$code)

    path <- pkgsimil_test_skeleton ()
    roxygen2::roxygenise (path)

    emb_fns <- with_mock_dir ("emb_raw_fns", {
        pkgsimil_embeddings_raw (path, functions_only = TRUE)
    })
    expect_embeddings_matrix (emb_fns)
})
