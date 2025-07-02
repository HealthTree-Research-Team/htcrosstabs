
#' @export
`var_name<-` <- function(ct_data, value) {
    UseMethod("var_name<-", ct_data)
}

#' @export
`var<-` <- function(ct_data, value) {
    UseMethod("var<-", ct_data)
}

#' @export
`var_levels<-` <- function(ct_data, value) {
    UseMethod("var_levels<-", ct_data)
}

#' @export
`cohort_name<-` <- function(ct_data, value) {
    UseMethod("cohort_name<-", ct_data)
}

#' @export
`cohort<-` <- function(ct_data, value) {
    UseMethod("cohort<-", ct_data)
}

#' @export
`cohort_levels<-` <- function(ct_data, value) {
    UseMethod("cohort_levels<-", ct_data)
}

#' @export
`var_mapping<-` <- function(ct_data, value) {
    UseMethod("var_mapping<-", ct_data)
}
