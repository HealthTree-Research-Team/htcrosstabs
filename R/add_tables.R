# TABLES ####
#' @export
add_default_table <- function(ct, round_mean_sd_to = ROUND_MEAN_SD_TO, round_med_iqr_to = ROUND_MED_IQR_TO, round_percent_to = ROUND_PERCENT_TO) {
    validate_input_add_default_table(ct, round_mean_sd_to, round_med_iqr_to, round_percent_to)

    if (is.crosstab.categorical(ct)) {
        ct |>
            add_total_row() |>
            add_count_rows(round_to = round_percent_to)
    } else if (is.crosstab.numeric(ct)) {
        ct |>
            add_total_row() |>
            add_mean_sd_row(round_to = round_mean_sd_to) |>
            add_med_iqr_row(round_to = round_med_iqr_to)
    } else if (is.crosstab.likert(ct)) {
        ct |>
            add_total_row() |>
            add_mean_sd_row(round_to = round_mean_sd_to) |>
            add_count_rows(round_to = round_percent_to)
    } else if (is.crosstab.multi(ct)) {
        ct |>
            add_total_row() |>
            add_count_rows(round_to = round_percent_to)
    } else {
        stop("Unrecognized crosstab type")
    }
}

#' @export
default_stacked_crosstab <- function(df, cohort_col_name = NULL, var_map = NULL, combined_cohort_name = "All", desc_col_name = "Description", round_mean_sd_to = ROUND_MEAN_SD_TO, round_med_iqr_to = ROUND_MED_IQR_TO, round_percent_to = ROUND_PERCENT_TO) {
    validate_input_default_stacked_crosstab(df, cohort_col_name, var_map)

    cols <- names(df)
    if (!is.null(cohort_col_name))
        cols <- cols[cols != cohort_col_name]

    matches_map <- function(col, var_map) {
        if (is.null(var_map)) return(FALSE)
        all(col %in% c(names(var_map), NA))
    }

    create_default_ct <- function(col) {
        temp_map <- if (matches_map(df[[col]], var_map)) var_map else NULL
        filtered_df <- df[, c(col, cohort_col_name), drop = F]
        add_default_table(
            crosstab(
                df = filtered_df,
                cohort_col_name = cohort_col_name,
                var_map = temp_map,
                combined_cohort_name = combined_cohort_name,
                desc_col_name = desc_col_name
            ),
            round_mean_sd_to = round_mean_sd_to,
            round_med_iqr_to = round_med_iqr_to,
            round_percent_to = round_percent_to
        )
    }

    cts <- lapply(cols, create_default_ct)
    cts <- do.call(stack_crosstabs, cts)
    return(cts)
}
