
# UTILITIES ####
#' @export
add_rows <- function(ct, rows) {
    validate_add_rows(ct, rows)

    # Combine rows (This also checks to make sure column names match)
    combined <- rbind(ct, rows)

    # Restore attributes (except dimensions, row.names, and column names)
    orig_attrs <- attributes(ct)
    to_restore <- setdiff(names(orig_attrs), c("row.names", "names", "class"))

    for (attr_name in to_restore)
        attr(combined, attr_name) <- orig_attrs[[attr_name]]

    # Restore class
    class(combined) <- orig_attrs[["class"]]

    return(combined)
}

#' @export
to_wide <- function(long_df, desc_col, cohort_col, na_fill = NA) {
    validate_to_wide(long_df, desc_col, cohort_col)

    # Get column names
    val_col <- names(long_df)[!(names(long_df) %in% c(desc_col, cohort_col))]

    # Reorder long_df
    long_df <- long_df[, c(desc_col, cohort_col, val_col), drop = FALSE]

    # Pivot wider
    wide_df <- long_df |>
        tidyr::pivot_wider(
            names_from = {{cohort_col}},
            values_from = {{val_col}},
            # Make a column for EVERY factor level,
            # even ones that aren't represented in the data
            names_expand = TRUE
        )

    # Convert to data frame to avoid weirdness
    wide_df <- data.frame(wide_df, check.names = F)

    # Fill NA values
    cohort_cols <- names(wide_df)[names(wide_df) != desc_col]
    wide_df[, cohort_cols][is.na(wide_df[, cohort_cols, drop = F])] <- na_fill

    return(wide_df)
}

#' @export
to_long <- function(wide_df, description_col, cohorts_to, values_to) {
    validate_to_long(wide_df, description_col, cohorts_to, values_to)

    cohort_cols <- names(wide_df)[names(wide_df) != description_col]

    long_df <- wide_df |>
        tidyr::pivot_longer(
            dplyr::all_of(cohort_cols),
            names_to = cohorts_to,
            values_to = values_to
        )

    long_df <- data.frame(long_df, check.names = F)
    long_df[[cohorts_to]] <- factor(long_df[[cohorts_to]], levels = cohort_cols)

    return(long_df)
}

# ROWS ####
#' @export
get_total_row <- function(ct, wide = T, long_out_col = COMP_TOT_COL_NAME) {
    validate_get_total_row(ct, wide, long_out_col)

    # Make sure there is no clash in the intermediate column names
    complete_col <- get_non_matching(COMP_COL_NAME, cohort_name(ct))
    total_col <- get_non_matching(TOTAL_COL_NAME, cohort_name(ct))
    if (wide) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))
    validate_cols(ct, long_out_col, wide)

    # Create string column
    totals <- get_complete(ct, out_col_name = complete_col) |>
        dplyr::full_join(get_total(ct, out_col_name = total_col), by = cohort_name(ct))
    totals[[long_out_col]] <- paste0(
        totals[[complete_col]], " / ", totals[[total_col]]
    )
    totals <- totals[, c(cohort_name(ct), long_out_col), drop = FALSE]

    # Add description column
    totals[[desc_name(ct)]] <- COMP_TOT_DESC
    totals <- totals[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (wide) totals <- to_wide(totals, desc_name(ct), cohort_name(ct))

    return(totals)
}

#' @export
add_total_row <- function(ct) {
    validate_add_total_row(ct)

    total_row <- get_total_row(ct, wide = T)
    ct <- add_rows(ct, total_row)

    return(ct)
}

#' @export
get_mean_sd_row <- function(ct, wide = T, long_out_col = MEAN_SD_COL_NAME, round_to = ROUND_MEAN_SD_TO) {
    validate_get_mean_sd_row(ct, wide, long_out_col, round_to)

    # Make sure there is no clash in the intermediate column names
    mean_col <- get_non_matching(MEAN_COL_NAME, cohort_name(ct))
    sd_col <- get_non_matching(SD_COL_NAME, cohort_name(ct))
    if (wide) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))
    validate_cols(ct, long_out_col, wide)

    # Create the string column
    mean_sds <- get_mean(ct, round_to = round_to, out_col_name = mean_col) |>
        dplyr::full_join(get_sd(ct, round_to = round_to, out_col_name = sd_col), by = cohort_name(ct))
    mean_sds[[long_out_col]] <- paste0(
        mean_sds[[mean_col]], " \u00b1 ", mean_sds[[sd_col]]
    )
    mean_sds <- mean_sds[, c(cohort_name(ct), long_out_col), drop = FALSE]

    # Add description column
    mean_sds[[desc_name(ct)]] <- MEAN_SD_DESC
    mean_sds <- mean_sds[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (wide) mean_sds <- to_wide(mean_sds, desc_name(ct), cohort_name(ct))

    return(mean_sds)
}

#' @export
add_mean_sd_row <- function(ct, round_to = ROUND_MEAN_SD_TO) {
    validate_add_mean_sd_row(ct, round_to)

    mean_sd_row <- get_mean_sd_row(ct, wide = T, round_to = round_to)
    ct <- add_rows(ct, mean_sd_row)

    return(ct)
}

#' @export
get_med_iqr_row <- function(ct, wide = T, long_out_col = MED_IQR_COL_NAME, round_to = ROUND_MED_IQR_TO) {
    validate_get_med_iqr_row(ct, wide, long_out_col, round_to)

    # Make sure there is no clash in the intermediate column names
    med_col <- get_non_matching(MED_COL_NAME, cohort_name(ct))
    q1_col <- get_non_matching(Q1_COL_NAME, cohort_name(ct))
    q3_col <- get_non_matching(Q3_COL_NAME, cohort_name(ct))
    if (wide) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))
    validate_cols(ct, long_out_col, wide)

    # Create the string column
    med_iqrs <- get_med(ct, round_to = round_to, out_col_name = med_col) |>
        dplyr::full_join(get_q1(ct, round_to = round_to, out_col_name = q1_col), by = cohort_name(ct)) |>
        dplyr::full_join(get_q3(ct, round_to = round_to, out_col_name = q3_col), by = cohort_name(ct))
    med_iqrs[[long_out_col]] <- paste0(
        med_iqrs[[med_col]], " (", med_iqrs[[q1_col]], ", ", med_iqrs[[q3_col]], ")"
    )
    med_iqrs <- med_iqrs[, c(cohort_name(ct), long_out_col), drop = FALSE]

    # Add description column
    med_iqrs[[desc_name(ct)]] <- MED_IQR_DESC
    med_iqrs <- med_iqrs[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (wide) med_iqrs <- to_wide(med_iqrs, desc_name(ct), cohort_name(ct))

    return(med_iqrs)
}

#' @export
add_med_iqr_row <- function(ct, round_to = MED_IQR_DESC) {
    validate_add_med_iqr_row(ct, round_to)

    med_iqr_row <- get_med_iqr_row(ct, wide = T, round_to = round_to)
    ct <- add_rows(ct, med_iqr_row)

    return(ct)
}

#' @export
get_count_rows <- function(ct, wide = T, long_out_col = COUNT_COL_NAME, round_to = ROUND_PERCENT_TO) {
    validate_get_count_rows(ct, wide, long_out_col, round_to)

    # Make sure there is no clash in the intermediate column names
    count_col <- get_non_matching(COUNT_COL_NAME, c(cohort_name(ct), var_name(ct)))
    comp_col <- get_non_matching(COMP_COL_NAME, c(cohort_name(ct), var_name(ct)))
    if (wide) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))
    validate_cols(ct, long_out_col, wide)

    # Create the string column
    counts <- get_counts(ct, out_col_name = count_col) |>
        dplyr::full_join(get_complete(ct, out_col_name = comp_col), by = cohort_name(ct))
    counts[[long_out_col]] <- paste0(
        counts[[count_col]], " (", round(100 * counts[[count_col]] / counts[[comp_col]], digits = round_to), "%)"
    )

    # Reorder and rename variable column to description column
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (wide) counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = "0 (0%)")

    return(counts)
}

#' @export
add_count_rows <- function(ct, round_to = ROUND_PERCENT_TO) {
    validate_add_count_rows(ct, round_to)

    count_rows <- get_count_rows(ct, wide = T, round_to = round_to)
    ct <- add_rows(ct, count_rows)

    return(ct)
}
