test_that ("tree similarity", {

    fns <- ls ("package:stats")
    trees <- lapply (fns, function (f) {
        get (f) |>
            deparse (width.cutoff = 500L) |>
            paste0 (collapse = "\n") |>
            tree_parse ()
    })
    trees <- trees [1:3]

    s <- tree_similarity (trees, num_cores = 1L) # Default
    expect_s3_class (s, "data.frame")
    expect_equal (ncol (s), 6L)
    nms <- c (
        "source_tree_index", "dest_tree_index", "source_tree_length",
        "dest_tree_length", "edit_distance", "tree_similarity"
    )
    expect_identical (names (s), nms)

    combs <- t (combn (seq_along (trees), m = 2L))
    expect_equal (nrow (s), nrow (combs))
    s12 <- as.matrix (s [, 1:2])
    dimnames (s12) <- NULL
    expect_identical (s12, combs)
    for (i in 3:5) {
        expect_type (s [, i], "double")
        expect_true (all (s [, i] > 0))
    }

    s2 <- tree_similarity (trees, num_cores = 2L)
    expect_identical (s, s2)
})
