foo <- function(obj) {
    UseMethod("foo", obj)
}

#' @method foo bar
#' @noRd
foo.bar <- function(obj) {
    print("foo.bar")
}

#' @method foo default
#' @noRd
foo.default <- function(obj) {
    print("foo.default")
}
