# IMPORTS ####
#' @import assertthat
validate_input_new_crosstab <- function(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name) {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name))
        assert_that(is.character(cohort_col_name))
    if (!is.null(var_map))
        assert_that(
            is.numeric(var_map),
            !is.null(names(var_map)),
            msg = "var_map must be a named vector of numeric values"
        )
    assert_that(is.character(combined_cohort_name))
    assert_that(is.character(desc_col_name))
}

# FUNCTIONS ####
validate_crosstab <- function(ct) {
    assert_crosstab(ct)
    assert_that(has_attr(ct, "data"))
    data <- attr(ct, "data")
    validate_crosstab_data(data)

    return(TRUE)
}

validate_input_data_table_getter <- function(ct, raw) {
    assert_crosstab(ct)
    assert_that(has_attr(ct, "data"))
    assert_that(is.logical(raw))
}

validate_input_data_table_setter <- function(ct, value) {
    assert_crosstab(ct)
    assert_crosstab_data(value)
}
