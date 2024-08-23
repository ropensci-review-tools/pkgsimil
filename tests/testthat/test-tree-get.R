test_that ("tree get", {

    expect_error (
        tree <- tree_get (),
        "is.character\\(pkg_name\\) is not TRUE"
    )
    expect_error (
        tree <- tree_get (character (0L)),
        "length\\(pkg_name\\) > 0L is not TRUE"
    )

    expect_error (
        tree <- tree_get ("not_a_package"),
        "pkg_name \\[not_a_package\\] is not a package"
    )

    expect_error (
        tree <- tree_get ("tools", "utils"),
        "is.logical\\(exported_only\\) is not TRUE"
    )

    pkg_name <- c ("tools", "utils")
    expect_silent (
        trees <- tree_get (pkg_name)
    )
    expect_type (trees, "character")
    expect_equal (length (trees), length (pkg_name))
    # Trees should be full of "(" and ")":
    huge_number <- 100000
    n_br <- t (vapply (trees, function (i) {
        c (
            length (gregexpr ("\\(", i) [[1]]),
            length (gregexpr ("\\)", i) [[1]])
        )
    }, integer (2L)))
    n_br_df <- data.frame (n_br)
    names (n_br_df) <- c ("open", "close")
    expect_true (all (n_br_df$open > huge_number))
    expect_true (all (n_br_df$close > huge_number))
    # Numbers of open and close should balance:
    expect_true (all (apply (n_br, 1, function (i) i [1] == i [2])))
    # Trees should be huge:
    n <- vapply (trees, nchar, integer (1L))
    expect_true (all (n > huge_number))

    # Test 'exported_only param:'
    trees_exp <- tree_get (pkg_name, exported_only = TRUE)
    expect_equal (length (trees_exp), length (trees))
    expect_true (all (nchar (trees) > nchar (trees_exp)))
})
