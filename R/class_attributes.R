#' Get Values from Stored Data
#'
#' @description
#' The getter functions will return different bits of data or return the data
#' in different formats as needed. They can be called on both crosstab and
#' crosstab_data objects.
#'
#' The setter functions will set those same values if provided with valid data.
#'
#' @param ct_data The crosstab or crosstab_data object you want to extract the data from
#' @param raw Logical, should the combined or "All" cohort data be removed?
#' @param value The replacement value
#'
#' @returns The specified data
#' @name class_attributes
#'
#' @examples
#' test_ct <- crosstab(
#'     length_by_species,
#'     "species",
#'     new_var_col_name = "orig_var_name"
#' )
#'
#' var_name(test_ct) # orig_var_name
#' var_name(test_ct) <- "new_var_name"
#' var_name(test_ct) # new_var_name
#'
NULL

#' @describeIn class_attributes Returns the name of the variable column
#' @export
var_name <- function(ct_data) {
    UseMethod("var_name", ct_data)
}

#' @describeIn class_attributes Returns the variable column as a vector
#' @export
var <- function(ct_data, raw = FALSE) {
    UseMethod("var", ct_data)
}

#' @describeIn class_attributes Returns the factor levels in the variable column
#' @export
var_levels <- function(ct_data) {
    UseMethod("var_levels", ct_data)
}

#' @describeIn class_attributes Returns the name of the cohort column
#' @export
cohort_name <- function(ct_data) {
    UseMethod("cohort_name", ct_data)
}

#' @describeIn class_attributes Returns the cohort column as a vector
#' @export
cohort <- function(ct_data, raw = FALSE) {
    UseMethod("cohort", ct_data)
}

#' @describeIn class_attributes Returns the factor levels in the cohort column
#' @export
cohort_levels <- function(ct_data, raw = FALSE) {
    UseMethod("cohort_levels", ct_data)
}

#' @describeIn class_attributes Returns the named numeric vector used to map likert values to their numeric value
#' @export
var_map <- function(ct_data) {
    UseMethod("var_map", ct_data)
}

#' @describeIn class_attributes Returns the variable column, except all values are mapped using var_map
#' @export
var_mapped <- function(ct_data, raw = FALSE) {
    UseMethod("var_mapped", ct_data)
}

#' @describeIn class_attributes Returns the name of the combined cohort added to grouped data
#' @export
combined_cohort_name <- function(ct_data) {
    UseMethod("combined_cohort_name", ct_data)
}

#' @describeIn class_attributes Returns the crosstab_data object without the extra combined cohort data
#' @export
get_raw_data <- function(ct_data) {
    UseMethod("get_raw_data", ct_data)
}

#' @describeIn class_attributes Returns the name of the leftmost description column in the output
#' @export
desc_name <- function(ct_data) {
    UseMethod("desc_name", ct_data)
}

#' @describeIn class_attributes Sets the name of the variable column
#' @export
`var_name<-` <- function(ct_data, value) {
    UseMethod("var_name<-", ct_data)
}

#' @describeIn class_attributes Sets the values in the variable column
#' @export
`var<-` <- function(ct_data, value) {
    UseMethod("var<-", ct_data)
}

#' @describeIn class_attributes Sets the factor levels in the variable column
#' @export
`var_levels<-` <- function(ct_data, value) {
    UseMethod("var_levels<-", ct_data)
}

#' @describeIn class_attributes Sets the name of the cohort column
#' @export
`cohort_name<-` <- function(ct_data, value) {
    UseMethod("cohort_name<-", ct_data)
}

#' @describeIn class_attributes Sets the values in the cohort column
#' @export
`cohort<-` <- function(ct_data, value) {
    UseMethod("cohort<-", ct_data)
}

#' @describeIn class_attributes Sets the factor levels in the cohort column
#' @export
`cohort_levels<-` <- function(ct_data, value) {
    UseMethod("cohort_levels<-", ct_data)
}

#' @describeIn class_attributes Sets the numeric values to map the character variables to
#' @export
`var_map<-` <- function(ct_data, value) {
    UseMethod("var_map<-", ct_data)
}
