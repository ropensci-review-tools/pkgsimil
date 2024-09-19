expected_embedding_length <- 768

test_that ("embeddings properties", {

    txt <- "test text"
    emb <- with_mock_dir ("embeddings", {
        get_embeddings (txt)
    })
    expect_type (emb, "double")
    expect_length (dim (emb), 2L)
    expect_equal (nrow (emb), expected_embedding_length)
    expect_true (min (emb) < 0)
    expect_true (max (emb) > 0)
})
