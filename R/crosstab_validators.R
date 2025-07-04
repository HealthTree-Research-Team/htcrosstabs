# IMPORTS ####
#' @import assertthat
validate_new_crosstab <- function(df, cohort_col_name, likert_map, combined_cohort_name, desc_col_name) {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name))
        assert_that(is.character(cohort_col_name))
    if (!is.null(likert_map))
        assert_that(
            is.numeric(likert_map),
            !is.null(names(likert_map)),
            msg = "likert_map must be a named vector of numeric values"
        )
    assert_that(is.character(combined_cohort_name))
    assert_that(is.character(desc_col_name))
}

validate_crosstab <- function(ct) {
    assert_crosstab(ct)
    assert_that(has_attr(ct, "data"))
    data <- attr(ct, "data")
    validate_crosstab_data(data)

    return(TRUE)
}

validate_set_data <- function(ct_data, value) {
    assert_crosstab(ct_data)
    assert_crosstab_data(value)
}

validate_get_data <- function(ct) {
    assert_crosstab(ct)
    assert_that(has_attr(ct, "data"))
}
