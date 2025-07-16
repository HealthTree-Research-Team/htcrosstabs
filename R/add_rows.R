# UTILITIES ####
#' @export
to_wide <- function(long_df, desc_col, cohort_col, na_fill = NA) {
    validate_input_to_wide(long_df, desc_col, cohort_col, na_fill)

    # Get column names
    val_cols <- names(long_df)[!(names(long_df) %in% c(desc_col, cohort_col))]

    # Reorder long_df
    long_df <- long_df[, c(desc_col, cohort_col, val_cols), drop = FALSE]

    # Pivot wider
    wide_df <- long_df |>
        tidyr::pivot_wider(
            names_from = {{cohort_col}},
            values_from = {{val_cols}},
            # Make a column for EVERY factor level,
            # even ones that aren't represented in the data
            names_expand = TRUE,
            names_sort = TRUE,
            values_fill = NA
        )

    # Convert to data frame to avoid weirdness
    wide_df <- data.frame(wide_df, check.names = F)

    # Fill NA values
    # I KNOW that pivot_wider has a values_fill option, but it requires that the
    # na_fill value match the data type of the rest of the column, whereas doing
    # it this way will cast the data types. It will all be converted to
    # characters anyway in the final output column.
    cohort_cols <- names(wide_df)[names(wide_df) != desc_col]
    wide_df[, cohort_cols][is.na(wide_df[, cohort_cols, drop = F])] <- na_fill

    return(wide_df)
}

#' @export
to_long <- function(wide_df, description_col, cohorts_to, values_to, convert_NA_cohort = TRUE) {
    validate_input_to_long(wide_df, description_col, cohorts_to, values_to, convert_NA_cohort)

    # Get the columns to pivot longer
    cohort_cols <- names(wide_df)[names(wide_df) != description_col]

    # Pivot longer
    long_df <- wide_df |>
        tidyr::pivot_longer(
            dplyr::all_of(cohort_cols),
            names_to = cohorts_to,
            values_to = values_to
        )

    if (convert_NA_cohort)
        long_df[[cohorts_to]][long_df[[cohorts_to]] == "NA"] <- NA

    # Convert to data frame and re-factorize
    long_df <- data.frame(long_df, check.names = F)
    long_df[[cohorts_to]] <- factor(long_df[[cohorts_to]], levels = cohort_cols)

    return(long_df)
}

#' @export
add_rows <- function(ct, rows) {
    validate_input_add_rows(ct, rows)

    # Combine rows
    combined <- dplyr::bind_rows(ct, rows)

    ncol_initial <- ncol(ct)
    ncol_final <- ncol(combined)
    if (ncol_initial != 0 & ncol_initial != ncol_final)
        warning("Column mismatch between ct and rows")

    # Restore attributes (except dimensions, row.names, and column names)
    orig_attrs <- attributes(ct)
    to_restore <- setdiff(names(orig_attrs), c("row.names", "names", "class"))
    for (attr_name in to_restore)
        attr(combined, attr_name) <- orig_attrs[[attr_name]]

    # Restore class
    class(combined) <- orig_attrs[["class"]]

    # Add one to the index
    cur_index <- index(combined)
    cur_index[length(cur_index)] <- cur_index[length(cur_index)] + nrow(rows)
    attr(combined, "index") <- cur_index

    return(combined)
}

# ROWS ####
#' @export
get_total_row <- function(ct, long_out_col = COMP_TOT_COL_NAME, long = F) {
    validate_input_get_total_row(ct, long, long_out_col)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get totals and add description column
    totals <- get_complete_total(ct, out_col_name = long_out_col)
    totals[[desc_name(ct)]] <- COMP_TOT_DESC
    totals <- totals[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) totals <- to_wide(totals, desc_name(ct), cohort_name(ct))
    return(totals)
}

#' @export
add_total_row <- function(ct) {
    validate_input_add_total_row(ct)

    # Get the row and add it to the running output table
    total_row <- get_total_row(ct, long = F)
    ct <- add_rows(ct, total_row)

    return(ct)
}

#' @export
get_mean_sd_row <- function(ct, round_to = ROUND_MEAN_SD_TO, long_out_col = MEAN_SD_COL_NAME, long = F) {
    validate_input_get_mean_sd_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    mean_sds <- get_mean_sd(ct, out_col_name = long_out_col, round_to = round_to)
    mean_sds[[desc_name(ct)]] <- MEAN_SD_DESC
    mean_sds <- mean_sds[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) mean_sds <- to_wide(mean_sds, desc_name(ct), cohort_name(ct))
    return(mean_sds)
}

#' @export
add_mean_sd_row <- function(ct, round_to = ROUND_MEAN_SD_TO) {
    validate_input_add_mean_sd_row(ct, round_to)

    # Get the row and add it to the running output table
    mean_sd_row <- get_mean_sd_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, mean_sd_row)

    return(ct)
}

#' @export
get_med_iqr_row <- function(ct, round_to = ROUND_MED_IQR_TO, long_out_col = MED_IQR_COL_NAME, long = F) {
    validate_input_get_med_iqr_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    med_iqrs <- get_med_iqr(ct, out_col_name = long_out_col, round_to = round_to)
    med_iqrs[[desc_name(ct)]] <- MED_IQR_DESC
    med_iqrs <- med_iqrs[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) med_iqrs <- to_wide(med_iqrs, desc_name(ct), cohort_name(ct))
    return(med_iqrs)
}

#' @export
add_med_iqr_row <- function(ct, round_to = ROUND_MED_IQR_TO) {
    validate_input_add_med_iqr_row(ct, round_to)

    # Get the row and add it to the running output column
    med_iqr_row <- get_med_iqr_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, med_iqr_row)

    return(ct)
}

#' @export
get_count_rows <- function(ct, round_to = ROUND_PERCENT_TO, long_out_col = COUNT_COL_NAME, long = F) {
    validate_input_get_count_rows(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    counts <- get_count_percent(ct, out_col_name = long_out_col, round_to = round_to)
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = "0 (0%)")
    return(counts)
}

#' @export
add_count_rows <- function(ct, round_to = ROUND_PERCENT_TO) {
    validate_input_add_count_rows(ct, round_to)

    # Get the rows and add it to the running output table
    count_rows <- get_count_rows(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, count_rows)

    return(ct)
}
