
validate_input_add_default_table <- function(ct, round_mean_sd_to, round_med_iqr_to, round_percent_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_med_iqr_to))
    assert_that(is.numeric(round_percent_to))
}

validate_input_auto_stacked_table <- function(df, cohort_col_name, var_map) {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name)) {
        assert_that(is.character(cohort_col_name))
        assert_that(cohort_col_name %in% names(df))
    }
    if (!is.null(var_map)) {
        assert_that(
            is.numeric(var_map), !is.null(names(var_map)),
            msg = "var_map must be a named numeric vector"
        )
    }

    assert_that(
        length(setdiff(names(df), cohort_col_name)) > 0,
        msg = "df must have at least 1 column aside from the cohort column (if applicable)"
    )
}
