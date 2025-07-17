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
    assert_that(has_attr(ct, "index"))
    index <- attr(ct, "index")
    assert_that(
        is.numeric(index),
        !is.null(names(index)),
        msg = "index must be a named vector of numeric values"
    )

    return(TRUE)
}

validate_input_data_table_getter <- function(ct, raw) {
    assert_crosstab(ct, strict = T)
    assert_that(has_attr(ct, "data"))
    assert_that(is.logical(raw))
}

validate_input_index_getter <- function(ct, long) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(long))
}

validate_input_data_table_setter <- function(ct, value) {
    assert_crosstab(ct, strict = T)
    assert_crosstab_data(value)

    assert_that(
        are_equal(is.crosstab.grouped(ct), is.crosstab.grouped(value)),
        msg = "If the original data is grouped by a cohort column, the new data must be too"
    )
    assert_that(
        are_equal(cohort_name(ct), cohort_name(value)),
        msg = "New data table must have the same cohort column as the original"
    )
    assert_that(
        are_equal(cohort_levels(ct), cohort_levels(value)),
        msg = "New data table must have the same cohort factor levels and combined_cohort_name as the original"
    )
    assert_that(
        are_equal(combined_cohort_name(ct), combined_cohort_name(value)),
        msg = "New data table must have the same combined_cohort_name as the original"
    )
    assert_that(
        are_equal(desc_name(ct), desc_name(value)),
        msg = "New data table must have the same desc_col_name as the original"
    )
}

validate_input_set_new_data_df <- function(ct, df, var_map) {
    assert_crosstab(ct, strict = T)
    assert_that(is.data.frame(df))
    if (!is.null(var_map)) {
        assert_that(
            is.numeric(var_map),
            !is.null(names(var_map)),
            msg = "var_map must be a named numeric vector"
        )
    }
}

validate_input_index_setter <- function(ct, value) {
    assert_crosstab(ct, strict = T)
    assert_that(
        is.numeric(value),
        !is.null(names(value)),
        msg = "new index must be a named numeric vector"
    )
    cur_index <- attr(ct, "index")
    assert_that(all(!is.na(cur_index)), msg = "Can not have NA values in index")
    assert_that(
        sum(cur_index) == sum(value),
        msg = "New index must have the same total number of rows (same sum) as the old index"
    )
}

validate_input_stack_crosstabs <- function(cts) {
    assert_that(
        all(sapply(cts, function(ct) is.crosstab(ct, strict = T))),
        msg = "All arguments must be crosstab objects"
    )
}
