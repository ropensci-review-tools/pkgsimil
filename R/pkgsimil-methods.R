#' Print method for 'pkgsimil' objects
#'
#' @param x Object to be printed
#' @param ... Not used
#'
#' @family utils
#' @export
print.pkgsimil <- function (x, ...) {

    n <- attr (x, "n")

    if (inherits (x, "data.frame")) {
        print (utils::head (x$package, n))
    } else if (inherits (x, "list")) {
        xout <- lapply (x, function (i) utils::head (i$package, n))
        print (xout)
    } else {
        print (x)
    }
}
