validate_input_add_categorical_table <- function(ct, keep_na_vars, round_percent_to) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(keep_na_vars))
    assert_that(is.numeric(round_percent_to))
}

validate_input_add_numeric_table <- function(ct, round_mean_sd_to, round_med_iqr_to) {
    assert_crosstab(ct, strict = T)
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_med_iqr_to))
}

validate_input_add_likert_table <- function(ct, keep_na_vars, round_mean_sd_to, round_percent_to) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(keep_na_vars))
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_percent_to))
}

validate_input_add_default_table <- function(ct, keep_na_vars, round_mean_sd_to, round_med_iqr_to, round_percent_to) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(keep_na_vars))
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_med_iqr_to))
    assert_that(is.numeric(round_percent_to))
}

validate_input_default_stacked_crosstab <- function(df, cohort_col_name, var_map) {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name)) {
        assert_that(is.character(cohort_col_name))
        assert_that(cohort_col_name %in% names(df), msg = sprintf(
            "Cohort column name \"%s\" not found in provided data frame",
            cohort_col_name
        ))
    }

    if (!is.null(var_map)) {
        if (is.list(var_map)) {
            sapply(var_map, function(map) {
                assert_that(
                    is.numeric(map), !is.null(names(map)),
                    msg = "If var_map is a list, all elements must be named numeric vectors"
                )
            })

            var_col_names <- setdiff(names(df), cohort_col_name)
            assert_that(
                all(names(var_map) %in% var_col_names),
                msg = "If var_map is a list, names must correspond to non-cohort columns that exist in df"
            )
        } else {
            assert_that(
                is.numeric(var_map), !is.null(names(var_map)),
                msg = "var_map must be either a named numeric vector or a list of named numeric vectors"
            )
        }
    }

    assert_that(
        length(setdiff(names(df), cohort_col_name)) > 0,
        msg = "df must have at least 1 column aside from the cohort column (if applicable)"
    )
}
