
#' @export
var_name <- function(ct_data) {
    UseMethod("var_name", ct_data)
}

#' @export
var <- function(ct_data) {
    UseMethod("var", ct_data)
}

#' @export
var_levels <- function(ct_data) {
    UseMethod("var_levels", ct_data)
}

#' @export
cohort_name <- function(ct_data) {
    UseMethod("cohort_name", ct_data)
}

#' @export
cohort <- function(ct_data) {
    UseMethod("cohort", ct_data)
}

#' @export
cohort_levels <- function(ct_data) {
    UseMethod("cohort_levels", ct_data)
}

#' @export
var_mapping <- function(ct_data) {
    UseMethod("var_mapping", ct_data)
}

#' @export
var_mapped <- function(ct_data, all = T) {
    UseMethod("var_mapped", ct_data)
}

#' @export
data_mapped <- function(ct_data) {
    UseMethod("data_mapped", ct_data)
}

#' @export
is_grouped <- function(ct_data) {
    UseMethod("is_grouped", ct_data)
}

#' @export
all_cohort_name <- function(ct_data) {
    UseMethod("all_cohort_name", ct_data)
}

#' @export
get_raw_data <- function(ct_data) {
    UseMethod("get_raw_data", ct_data)
}
