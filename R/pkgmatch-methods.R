#' Print method for 'pkgmatch' objects
#'
#' @param x Object to be printed
#' @param ... Not used
#'
#' @family utils
#' @export
print.pkgmatch <- function (x, ...) {

    n <- attr (x, "n")

    if ("rank" %in% names (x)) {
        xout <- x$package [seq_len (n)]
    } else {
        xout <- list (
            "text" = x$package [x$text_rank] [seq_len (n)],
            "code" = x$package [x$code_rank] [seq_len (n)]
        )
    }
    print (xout)
}

#' Head method for 'pkgmatch' objects
#'
#' @param x Object for which head is to be printed
#' @param n Number of rows of full `pkgmatch` object to be displayed
#' @param ... Not used
#'
#' @family utils
#' @export
head.pkgmatch <- function (x, n = 5L, ...) {
    head (as.data.frame (x), n = n)
}
