test_that ("parse", {
    code <- simpleError |> # Note no `()` to get and not call function.
        deparse (width.cutoff = 500L) |>
        paste0 (collapse = "\n")

    expect_type (code, "character")
    expect_equal (length (code), 1L)

    tree <- tree_parse (code, node_brackets = TRUE)

    expect_type (tree, "character")
    expect_equal (length (tree), 1L)
    n_open <- length (gregexpr ("\\(", tree) [[1]])
    n_close <- length (gregexpr ("\\)", tree) [[1]])
    expect_equal (n_open, n_close)

    tree_f <- tree_parse (code, node_brackets = FALSE)

    expect_type (tree_f, "character")
    expect_equal (length (tree_f), 1L)
    n_open_f <- length (gregexpr ("\\(", tree_f) [[1]])
    n_close_f <- length (gregexpr ("\\)", tree_f) [[1]])
    expect_equal (n_open, n_close)

    expect_true (n_open_f < n_open)
    expect_true (n_close_f < n_close)
})
