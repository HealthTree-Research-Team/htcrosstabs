# IMPORTS
#' @import assertthat
#' @importFrom rlang .data

# FUNCTIONS
validate_input_to_wide <- function(long_df, description_col, cohort_col, na_fill) {
    assert_that(is.data.frame(long_df))
    assert_that(is.character(description_col))
    assert_that(is.character(cohort_col))
    assert_that(!is.null(na_fill), msg = "na_fill can not be NULL")
    assert_that(
        ncol(long_df) == 3,
        msg = "long_df must have 3 columns: 1) description, 2) cohort, and 3) values"
    )
    assert_that(
        description_col %in% names(long_df),
        msg = "Description column name not found in long_df"
    )
    assert_that(
        cohort_col %in% names(long_df),
        msg = "Cohort column name not found in long_df"
    )
    assert_that(
        is.factor(long_df[[cohort_col]]),
        msg = "Cohort column must be a factor"
    )

    duplicate_vals <- long_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(description_col, cohort_col)))) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        # I have to use this notation instead of n > 1L because the checker
        # (run with check()) throws a fit saying, "Where did you declare this?"
        dplyr::filter(.data[["n"]] > 1L) |>
        nrow()

    assert_that(duplicate_vals == 0, msg = sprintf(
        "detected multiple rows with the same combination of values in %s and %s, cannot pivot to wide",
        description_col,
        cohort_col
    ))
}

validate_input_to_long <- function(wide_df, description_col, cohorts_to, values_to, convert_NA_cohort) {
    assert_that(is.data.frame(wide_df))
    assert_that(is.character(description_col))
    assert_that(is.character(cohorts_to))
    assert_that(is.character(values_to))
    assert_that(is.logical(convert_NA_cohort))
    assert_that(
        description_col %in% names(wide_df),
        msg = "Description column name not found in wide_df"
    )
    assert_that(
        !any(duplicated(c(description_col, cohorts_to, values_to))),
        msg = "description_col, cohorts_to, and values_to must all be unique"
    )
}

validate_input_add_rows <- function(ct, rows, index, index_from, prefer_table) {
    assert_crosstab(ct)
    assert_that(is.data.frame(rows))
    if (!is.null(index))
        assert_that(
            is.numeric(index),
            index >= 1,
            length(index) == 1,
            msg = "index must be a single positive integer"
        )
    assert_that(is.character(index_from))
    assert_that(length(index_from) == 1, msg = "index_from must only have one value")
    assert_that(index_from %in% c("top", "bottom"), msg = "index_from must be either \"top\" or \"bottom\"")
    assert_that(is.character(prefer_table))
    assert_that(length(prefer_table) == 1, msg = "prefer_table must only have one value")
    assert_that(prefer_table %in% c("top", "bottom"), msg = "prefer_table must be either \"top\" or \"bottom\"")
}

validate_input_col_names <- function(ct, long_out_col, long) {
    if (!long) {
        assert_that(
            !(desc_name(ct) %in% c(cohort_levels(ct), var_name(ct))),
            msg = "If long = FALSE, desc_col_name must be different than cohort_levels and var_name. Specify a different desc_col_name when creating the original crosstab object."
        )
    } else {
        assert_that(
            !(desc_name(ct) %in% c(cohort_name(ct), var_name(ct), long_out_col)),
            msg = "If long = TRUE, description column must be different from cohort_col, var_name, and long_out_col. Specify a different long_out_col in this function or a different desc_col_name when creating the original crosstab object."
        )
    }
}

validate_input_get_total_row <- function(ct, long, long_out_col) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
}

validate_input_add_total_row <- function(ct) {
    assert_crosstab(ct)
}

validate_input_get_complete_row <- function(ct, long, long_out_col) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
}

validate_input_add_complete_row <- function(ct) {
    assert_crosstab(ct)
}

validate_input_get_complete_total_row <- function(ct, long, long_out_col) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
}

validate_input_add_complete_total_row <- function(ct) {
    assert_crosstab(ct)
}

validate_input_get_mean_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_mean_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_sd_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_sd_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_mean_sd_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_mean_sd_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_med_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_med_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_q1_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_q1_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_q3_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_q3_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_q1_q3_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_q1_q3_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_iqr_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_iqr_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_iqr_q3_q1_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_iqr_q3_q1_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_med_q1_q3_row <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_med_q1_q3_row <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_count_rows <- function(ct, long, long_out_col) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
}

validate_input_add_count_rows <- function(ct) {
    assert_crosstab(ct)
}

validate_input_get_prop_rows <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_prop_rows <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_count_prop_rows <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_count_prop_rows <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_percent_rows <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_percent_rows <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}

validate_input_get_count_percent_rows <- function(ct, long, long_out_col, round_to) {
    assert_crosstab(ct)
    assert_that(is.logical(long))
    assert_that(is.character(long_out_col))
    assert_that(is.numeric(round_to))
}

validate_input_add_count_percent_rows <- function(ct, round_to) {
    assert_crosstab(ct)
    assert_that(is.numeric(round_to))
}
