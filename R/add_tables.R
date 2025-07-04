# TABLES ####
#' @export
add_default_table <- function(ct, round_mean_sd_to = ROUND_MEAN_SD_TO, round_med_iqr_to = ROUND_MED_IQR_TO, round_percent_to = ROUND_PERCENT_TO) {
    validate_add_default_table(ct, round_mean_sd_to, round_med_iqr_to, round_percent_to)

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
auto_stacked_table <- function(df, cohort_col_name = NULL, likert_map = NULL) {
    # cols <- names(df)
    # if (!is.null(cohort_col_name)) cols <- cols[cols != cohort_col_name]
    #
    # ct <-

    # Grouping??
}
