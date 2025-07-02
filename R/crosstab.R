# IMPORTS ####
#' @import assertthat

# CONSTANTS ####
CT_CLASS <- "crosstab"

# CONSTRUCTORS ####
new_crosstab <- function(df, cohort_col_name = NULL, likert_map = NULL, default_cohort = "All", default_response_col = "Response") {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name))
        assert_that(is.character(cohort_col_name))
    if (!is.null(likert_map))
        assert_that(
            is.numeric(likert_map),
            !is.null(names(likert_map)),
            msg = "likert_map must be a named vector of numeric values"
        )
    assert_that(is.character(default_cohort))

    data <- crosstab_data(
        df = df,
        cohort_col_name = cohort_col_name,
        likert_map = likert_map,
        default_cohort = default_cohort
    )

    classes <- c(CT_CLASS, class(data.frame()))

    structure(
        data.frame(), # Output table
        data = data,   # crosstab_data
        default_response_col = default_response_col,
        class = classes
    )
}

# VALIDATORS ####
validate_crosstab <- function(ct) {
    assert_that(inherits(ct, CT_CLASS))
    assert_that(has_attr(ct, "data"))
    data <- attr(ct, "data")
    validate_crosstab_data(data)

    assert_that(has_attr(ct, "default_response_col"))
    default_response_col <- attr(ct, "default_response_col")
    assert_that(
        !(default_response_col %in% cohort(data)),
        msg = "If \"Response\" is already one of the cohort names, please select a different default with default_response_col = [new default]"
    )

    return(TRUE)
}

# HELPERS ####
#' @export
crosstab <- function(df, cohort_col_name = NULL, likert_map = NULL, default_cohort = "All") {
    ct <- new_crosstab(
        df = df,
        cohort_col_name = cohort_col_name,
        likert_map = likert_map,
        default_cohort = default_cohort
    )

    validate_crosstab(ct)
    return(ct)
}

# GETTERS ####
#' @export
data <- function(ct) {
    assert_that(inherits(ct, CT_CLASS))
    assert_that(has_attr(ct, "data"))
    attr(ct, "data")
}

#' @export
response_col_name <- function(ct) {
    assert_that(inherits(ct, CT_CLASS))
    assert_that(has_attr(ct, "default_response_col"))
    attr(ct, "default_response_col")
}

#' @export
var_name.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    var_name(data(ct_data))
}

#' @export
var.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    var(data(ct_data))
}

#' @export
var_levels.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    var_levels(data(ct_data))
}

#' @export
cohort_name.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    cohort_name(data(ct_data))
}

#' @export
cohort.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    cohort(data(ct_data))
}

#' @export
cohort_levels.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    cohort_levels(data(ct_data))
}

#' @export
var_mapping.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    var_mapping(data(ct_data))
}

#' @export
var_mapped.crosstab <- function(ct_data, all = T) {
    assert_that(has_attr(ct_data, "data"))
    var_mapped(data(ct_data))
}

#' @export
data_mapped.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    data_mapped(data(ct_data))
}

#' @export
is_grouped.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    is_grouped(data(ct_data))
}

#' @export
all_cohort_name.crosstab <- function(ct_data) {
    assert_that(has_attr(ct_data, "data"))
    all_cohort_name(data(ct_data))
}

#' @export
get_raw_data.crosstab <- function(ct) {
    assert_that(has_attr(ct, "data"))
    get_raw_data(data(ct))
}

# SETTERS ####
#' @export
`data<-` <- function(ct, value) {
    assert_that(inherits(ct, CT_CLASS))
    assert_that(inherits(value, CT_DATA_CLASS))
    attr(ct, "data") <- value
    return(ct)
}

#' @export
`var_name<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `var_name<-`(data(ct_data), value)
}

#' @export
`var<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `var<-`(data(ct_data), value)
}

#' @export
`var_levels<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `var_levels<-`(data(ct_data), value)
}

#' @export
`cohort_name<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `cohort_name<-`(data(ct_data), value)
}

#' @export
`cohort<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `cohort<-`(data(ct_data), value)
}

#' @export
`cohort_levels<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `cohort_levels<-`(data(ct_data), value)
}

#' @export
`var_mapping<-.crosstab` <- function(ct_data, value) {
    assert_that(has_attr(ct_data, "data"))
    `var_mapping<-`(data(ct_data), value)
}
