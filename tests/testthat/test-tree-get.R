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
        "unused argument"
    )

    pkg_name <- c ("tools", "utils")
    expect_silent (
        trees <- tree_get (pkg_name)
    )
    expect_type (trees, "character")
    expect_equal (length (trees), length (pkg_name))
    # Trees should be full of "(" and ")":
    huge_number <- 100000
    n_br <- data.frame (t (vapply (trees, function (i) {
        c (
            length (gregexpr ("\\(", i) [[1]]),
            length (gregexpr ("\\)", i) [[1]])
        )
    }, integer (2L))))
    names (n_br) <- c ("open", "close")
    expect_true (all (n_br$open > huge_number))
    expect_true (all (n_br$close > huge_number))
    # Numbers of open and close should balance:
    expect_true (all (apply (n_br, 1, function (i) i [1] == i [2])))
    # Trees should be huge:
    n <- vapply (trees, nchar, integer (1L))
    expect_true (all (n > huge_number))
})
