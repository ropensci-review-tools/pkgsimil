#' Call the C++ function
#'
#' @param x Input numeric vector
#' @return Modified version of input
#' @export
test_cpp <- function (x = 1) {
    cpp_test_fn (x)
}
