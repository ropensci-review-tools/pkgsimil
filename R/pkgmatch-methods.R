#' Print method for 'pkgmatch' objects
#'
#' @param x Object to be printed
#' @param ... Not used
#'
#' @family utils
#' @export
print.pkgmatch <- function (x, ...) {

    n <- attr (x, "n")

    if (inherits (x, "data.frame")) {
        if ("rank" %in% names (x)) {
            xout <- x$package [seq_len (n)]
        } else {
            xout <- list (
                "text" = x$package [x$text_rank] [seq_len (n)],
                "code" = x$package [x$code_rank] [seq_len (n)]
            )
        }
        print (xout)
    } else if (inherits (x, "list")) {
        xout <- lapply (x, function (i) utils::head (i$package, n))
        print (xout)
    } else {
        print (x)
    }
}
