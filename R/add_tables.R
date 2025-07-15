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
auto_stacked_table <- function(df, cohort_col_name = NULL, var_map = NULL, combined_cohort_name = "All", desc_col_name = "Description", round_mean_sd_to = ROUND_MEAN_SD_TO, round_med_iqr_to = ROUND_MED_IQR_TO, round_percent_to = ROUND_PERCENT_TO) {
    validate_input_auto_stacked_table(df, cohort_col_name, var_map)

    # Since crosstab() and crosstab_data() use whether or not var_map is NULL
    # to differentiate categorical from likert, this function helps tell if
    # var_map should be passed to the function or if it should be NULL.
    matches_map <- function(col, var_map) {
        if (is.null(var_map)) return(FALSE)
        return(all(col %in% c(names(var_map), NA)))
    }

    # Get list of columns without cohort column as well as first column
    cols <- names(df)
    if (!is.null(cohort_col_name))
        cols <- cols[cols != cohort_col_name]
    first_col <- cols[1]

    # Loop through every variable column
    ct <- NULL
    for (col in cols) {

        # Select only the variable column and cohort column (if applicable)
        temp_df <- df[, c(col, cohort_col_name), drop = F]

        # var_map if it matches this variable column, NULL otherwise
        temp_map <- if (matches_map(temp_df[[col]], var_map)) var_map else NULL

        if (col == first_col) {
            # Create the original crosstab object for the first column
            ct <- crosstab(
                df = temp_df,
                cohort_col_name = cohort_col_name,
                var_map = temp_map,
                combined_cohort_name = combined_cohort_name,
                desc_col_name = desc_col_name
            )
        } else {
            # For all other columns, replace the data attribute in the existing crosstab
            data_table(ct) <- crosstab_data(
                df = temp_df,
                cohort_col_name = cohort_col_name,
                var_map = temp_map,
                combined_cohort_name = combined_cohort_name,
                desc_col_name = desc_col_name
            )
        }

        # Add the appropriate table to the running output table
        ct <- add_default_table(
            ct = ct,
            round_mean_sd_to = round_mean_sd_to,
            round_med_iqr_to = round_med_iqr_to,
            round_percent_to = round_percent_to
        )
    }

    # Return the final object
    return(ct)
}
