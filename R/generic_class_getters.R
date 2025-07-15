
#' @export
var_name <- function(ct_data) {
    UseMethod("var_name", ct_data)
}

#' @export
var <- function(ct_data, raw = F) {
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
cohort <- function(ct_data, raw = F) {
    UseMethod("cohort", ct_data)
}

#' @export
cohort_levels <- function(ct_data, raw = F) {
    UseMethod("cohort_levels", ct_data)
}

#' @export
var_map <- function(ct_data) {
    UseMethod("var_map", ct_data)
}

#' @export
var_mapped <- function(ct_data, raw = F) {
    UseMethod("var_mapped", ct_data)
}

#' @export
is_grouped <- function(ct_data) {
    UseMethod("is_grouped", ct_data)
}

#' @export
combined_cohort_name <- function(ct_data) {
    UseMethod("combined_cohort_name", ct_data)
}

#' @export
get_raw_data <- function(ct_data) {
    UseMethod("get_raw_data", ct_data)
}

#' @export
desc_name <- function(ct_data) {
    UseMethod("desc_name", ct_data)
}
