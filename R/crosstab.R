
# CONSTRUCTORS ####
new_crosstab <- function(df, cohort_col_name, likert_map, combined_cohort_name, desc_col_name) {
    validate_new_crosstab(df, cohort_col_name, likert_map, combined_cohort_name, desc_col_name)

    data <- crosstab_data(
        df = df,
        cohort_col_name = cohort_col_name,
        likert_map = likert_map,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name
    )

    structure(
        data.frame(), # Output table
        data = data,  # crosstab_data
        class = c(CT_CLASS, class(data.frame()))
    )
}

# HELPERS ####
#' @export
crosstab <- function(df, cohort_col_name = NULL, likert_map = NULL, combined_cohort_name = "All", desc_col_name = "Description") {
    ct <- new_crosstab(
        df = df,
        cohort_col_name = cohort_col_name,
        likert_map = likert_map,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name
    )

    validate_crosstab(ct)
    return(ct)
}

# GETTERS ####
#' @export
get_data <- function(ct) {
    validate_get_data(ct)
    attr(ct, "data")
}

#' @export
desc_name.crosstab <- function(ct_data) {
    desc_name(get_data(ct_data))
}

#' @export
var_name.crosstab <- function(ct_data) {
    var_name(get_data(ct_data))
}

#' @export
var.crosstab <- function(ct_data) {
    var(get_data(ct_data))
}

#' @export
var_levels.crosstab <- function(ct_data) {
    var_levels(get_data(ct_data))
}

#' @export
cohort_name.crosstab <- function(ct_data) {
    cohort_name(get_data(ct_data))
}

#' @export
cohort.crosstab <- function(ct_data) {
    cohort(get_data(ct_data))
}

#' @export
cohort_levels.crosstab <- function(ct_data) {
    cohort_levels(get_data(ct_data))
}

#' @export
var_mapping.crosstab <- function(ct_data) {
    var_mapping(get_data(ct_data))
}

#' @export
var_mapped.crosstab <- function(ct_data, all = T) {
    var_mapped(get_data(ct_data))
}

#' @export
combined_cohort_name.crosstab <- function(ct_data) {
    combined_cohort_name(get_data(ct_data))
}

#' @export
get_raw_data.crosstab <- function(ct_data) {
    get_raw_data(get_data(ct_data))
}

# SETTERS ####
#' @export
`set_data<-` <- function(ct_data, value) {
    validate_set_data(ct_data, value)
    attr(ct_data, "data") <- value
    return(ct_data)
}

#' @export
`var_name<-.crosstab` <- function(ct_data, value) {
    `var_name<-`(get_data(ct_data), value)
}

#' @export
`var<-.crosstab` <- function(ct_data, value) {
    `var<-`(get_data(ct_data), value)
}

#' @export
`var_levels<-.crosstab` <- function(ct_data, value) {
    `var_levels<-`(get_data(ct_data), value)
}

#' @export
`cohort_name<-.crosstab` <- function(ct_data, value) {
    `cohort_name<-`(get_data(ct_data), value)
}

#' @export
`cohort<-.crosstab` <- function(ct_data, value) {
    `cohort<-`(get_data(ct_data), value)
}

#' @export
`cohort_levels<-.crosstab` <- function(ct_data, value) {
    `cohort_levels<-`(get_data(ct_data), value)
}

#' @export
`var_mapping<-.crosstab` <- function(ct_data, value) {
    `var_mapping<-`(get_data(ct_data), value)
}
