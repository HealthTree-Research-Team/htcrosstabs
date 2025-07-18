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

# COMPLETE ROW ####
#' @export
get_complete_row <- function(ct, long = F, long_out_col = COMP_COL_NAME) {
    validate_input_get_complete_row(ct, long, long_out_col)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get completes and add description column
    completes <- get_complete(ct, out_col_name = long_out_col)
    completes[[desc_name(ct)]] <- COMP_DESC
    completes <- completes[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) completes <- to_wide(completes, desc_name(ct), cohort_name(ct))
    return(completes)
}

#' @export
add_complete_row <- function(ct) {
    validate_input_add_complete_row(ct)

    # Get the row and add it to the running output table
    complete_row <- get_complete_row(ct, long = F)
    ct <- add_rows(ct, complete_row)

    return(ct)
}

# TOTAL ROW ####
#' @export
get_total_row <- function(ct, long = F, long_out_col = TOTAL_COL_NAME) {
    validate_input_get_total_row(ct, long, long_out_col)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get totals and add description column
    totals <- get_total(ct, out_col_name = long_out_col)
    totals[[desc_name(ct)]] <- TOT_DESC
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

# COMPLETE AND TOTAL ROW ####
#' @export
get_complete_total_row <- function(ct, long = F, long_out_col = COMP_TOT_COL_NAME) {
    validate_input_get_complete_total_row(ct, long, long_out_col)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get complete and totals and add description column
    totals <- get_complete_total(ct, out_col_name = long_out_col)
    totals[[desc_name(ct)]] <- COMP_TOT_DESC
    totals <- totals[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) totals <- to_wide(totals, desc_name(ct), cohort_name(ct))
    return(totals)
}

#' @export
add_complete_total_row <- function(ct) {
    validate_input_add_complete_total_row(ct)

    # Get the row and add it to the running output table
    comp_total_row <- get_complete_total_row(ct, long = F)
    ct <- add_rows(ct, comp_total_row)

    return(ct)
}

# MEAN ROW ####
#' @export
get_mean_row <- function(ct, round_to = MEAN_ROUND_TO, long = F, long_out_col = MEAN_COL_NAME) {
    validate_input_get_mean_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    means <- get_mean(ct, out_col_name = long_out_col, round_to = round_to)
    means[[desc_name(ct)]] <- MEAN_DESC
    means <- means[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) means <- to_wide(means, desc_name(ct), cohort_name(ct))
    return(means)
}

#' @export
add_mean_row <- function(ct, round_to = MEAN_ROUND_TO) {
    validate_input_add_mean_row(ct, round_to)

    # Get the row and add it to the running output table
    mean_row <- get_mean_row(ct, round_to = round_to, long = F)
    ct <- add_rows(ct, mean_row)

    return(ct)
}

# SD ROW ####
#' @export
get_sd_row <- function(ct, round_to = ROUND_SD_TO, long = F, long_out_col = SD_COL_NAME) {
    validate_input_get_sd_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    sds <- get_sd(ct, out_col_name = long_out_col, round_to = round_to)
    sds[[desc_name(ct)]] <- SD_DESC
    sds <- sds[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) sds <- to_wide(sds, desc_name(ct), cohort_name(ct))
    return(sds)
}

#' @export
add_sd_row <- function(ct, round_to = ROUND_SD_TO) {
    validate_input_add_sd_row(ct, round_to)

    # Get the row and add it to the running output table
    sd_row <- get_sd_row(ct, round_to = round_to, long = F)
    ct <- add_rows(ct, sd_row)

    return(ct)
}

# MEAN AND SD ROW ####
#' @export
get_mean_sd_row <- function(ct, round_to = ROUND_MEAN_SD_TO, long = F, long_out_col = MEAN_SD_COL_NAME) {
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

# MEDIAN ROW ####
#' @export
get_median_row <- function(ct, round_to = ROUND_MEDIAN_TO, long = F, long_out_col = MED_COL_NAME) {
    get_med_row(ct, round_to = round_to, long = long, long_out_col = long_out_col)
}

#' @export
add_median_row <- function(ct, round_to = ROUND_MEDIAN_TO) {
    add_med_row(ct, round_to = round_to)
}

#' @export
get_med_row <- function(ct, round_to = ROUND_MEDIAN_TO, long = F, long_out_col = MED_COL_NAME) {
    validate_input_get_med_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    meds <- get_med(ct, out_col_name = long_out_col, round_to = round_to)
    meds[[desc_name(ct)]] <- MED_DESC
    meds <- meds[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) meds <- to_wide(meds, desc_name(ct), cohort_name(ct))
    return(meds)
}

#' @export
add_med_row <- function(ct, round_to = ROUND_MEDIAN_TO) {
    validate_input_add_med_row(ct, round_to)

    # Get the row and add it to the running output column
    med_row <- get_med_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, med_row)

    return(ct)
}

# Q1 ROW ####
#' @export
get_q1_row <- function(ct, round_to = ROUND_Q1_TO, long = F, long_out_col = Q1_COL_NAME) {
    validate_input_get_q1_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    q1s <- get_q1(ct, out_col_name = long_out_col, round_to = round_to)
    q1s[[desc_name(ct)]] <- Q1_DESC
    q1s <- q1s[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) q1s <- to_wide(q1s, desc_name(ct), cohort_name(ct))
    return(q1s)
}

#' @export
add_q1_row <- function(ct, round_to = ROUND_Q1_TO) {
    validate_input_add_q1_row(ct, round_to)

    # Get the row and add it to the running output column
    q1_row <- get_q1_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, q1_row)

    return(ct)
}

# Q3 ROW ####
#' @export
get_q3_row <- function(ct, round_to = ROUND_Q3_TO, long = F, long_out_col = Q3_COL_NAME) {
    validate_input_get_q3_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    q3s <- get_q3(ct, out_col_name = long_out_col, round_to = round_to)
    q3s[[desc_name(ct)]] <- Q3_DESC
    q3s <- q3s[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) q3s <- to_wide(q3s, desc_name(ct), cohort_name(ct))
    return(q3s)
}

#' @export
add_q3_row <- function(ct, round_to = ROUND_Q3_TO) {
    validate_input_add_q3_row(ct, round_to)

    # Get the row and add it to the running output column
    q3_row <- get_q3_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, q3_row)

    return(ct)
}

# Q1-Q3 ROW ####
#' @export
get_q1_q3_row <- function(ct, round_to = ROUND_Q1_Q3_TO, long = F, long_out_col = Q1_Q3_COL_NAME) {
    validate_input_get_q1_q3_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    q1_q3s <- get_q1_q3(ct, out_col_name = long_out_col, round_to = round_to)
    q1_q3s[[desc_name(ct)]] <- Q1_Q3_DESC
    q1_q3s <- q1_q3s[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) q1_q3s <- to_wide(q1_q3s, desc_name(ct), cohort_name(ct))
    return(q1_q3s)
}

#' @export
add_q1_q3_row <- function(ct, round_to = ROUND_Q1_Q3_TO) {
    validate_input_add_q1_q3_row(ct, round_to)

    # Get the row and add it to the running output column
    q1_q3_row <- get_q1_q3_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, q1_q3_row)

    return(ct)
}

# IQR ROW ####
#' @export
get_iqr_row <- function(ct, round_to = ROUND_IQR_TO, long = F, long_out_col = IQR_COL_NAME) {
    validate_input_get_iqr_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    iqrs <- get_iqr(ct, out_col_name = long_out_col, round_to = round_to)
    iqrs[[desc_name(ct)]] <- IQR_DESC
    iqrs <- iqrs[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) iqrs <- to_wide(iqrs, desc_name(ct), cohort_name(ct))
    return(iqrs)
}

#' @export
add_iqr_row <- function(ct, round_to = ROUND_IQR_TO) {
    validate_input_add_iqr_row(ct, round_to)

    # Get the row and add it to the running output column
    iqr_row <- get_iqr_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, iqr_row)

    return(ct)
}

# IQR Q3-Q1 ROW ####
#' @export
get_iqr_q3_q1_row <- function(ct, round_to = ROUND_IQR_Q3_Q1_TO, long = F, long_out_col = IQR_Q3_Q1_COL_NAME) {
    validate_input_get_iqr_q3_q1_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    iqr_q3_q1s <- get_iqr_q3_q1(ct, out_col_name = long_out_col, round_to = round_to)
    iqr_q3_q1s[[desc_name(ct)]] <- IQR_Q3_Q1_DESC
    iqr_q3_q1s <- iqr_q3_q1s[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) iqr_q3_q1s <- to_wide(iqr_q3_q1s, desc_name(ct), cohort_name(ct))
    return(iqr_q3_q1s)
}

#' @export
add_iqr_q3_q1_row <- function(ct, round_to = ROUND_IQR_Q3_Q1_TO) {
    validate_input_add_iqr_q3_q1_row(ct, round_to)

    # Get the row and add it to the running output column
    iqr_q3_q1_row <- get_iqr_q3_q1_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, iqr_q3_q1_row)

    return(ct)
}

# MEDIAN AND Q1-Q3 ROW ####
#' @export
get_med_q1_q3_row <- function(ct, round_to = ROUND_MED_Q1_Q3_TO, long = F, long_out_col = MED_Q1_Q3_COL_NAME) {
    validate_input_get_med_q1_q3_row(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))

    # Get values and add description column
    med_q1_q3s <- get_med_q1_q3(ct, out_col_name = long_out_col, round_to = round_to)
    med_q1_q3s[[desc_name(ct)]] <- MED_Q1_Q3_DESC
    med_q1_q3s <- med_q1_q3s[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) med_q1_q3s <- to_wide(med_q1_q3s, desc_name(ct), cohort_name(ct))
    return(med_q1_q3s)
}

#' @export
add_med_q1_q3_row <- function(ct, round_to = ROUND_MED_Q1_Q3_TO) {
    validate_input_add_med_q1_q3_row(ct, round_to)

    # Get the row and add it to the running output column
    med_q1_q3_row <- get_med_q1_q3_row(ct, long = F, round_to = round_to)
    ct <- add_rows(ct, med_q1_q3_row)

    return(ct)
}

# COUNT ROWS ####
#' @export
get_count_rows <- function(ct, long = F, long_out_col = COUNT_COL_NAME, keep_na_vars = F) {
    validate_input_get_count_rows(ct, long, long_out_col)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    counts <- get_count(ct, out_col_name = long_out_col, keep_na_vars = keep_na_vars)
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = "0 (0%)")
    return(counts)
}

#' @export
add_count_rows <- function(ct, keep_na_vars = F) {
    validate_input_add_count_rows(ct)

    # Get the rows and add it to the running output table
    count_rows <- get_count_rows(ct, long = F, keep_na_vars = keep_na_vars)
    ct <- add_rows(ct, count_rows)

    return(ct)
}

# PERCENT ROWS ####
#' @export
get_percent_rows <- function(ct, round_to = ROUND_PERCENT_TO, long = F, long_out_col = PERCENT_COL_NAME, keep_na_vars = F, raw = F) {
    validate_input_get_percent_rows(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    percents <- get_percent(ct, out_col_name = long_out_col, round_to = round_to, keep_na_vars = keep_na_vars, raw = raw)
    names(percents)[names(percents) == var_name(ct)] <- desc_name(ct)
    percents <- percents[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) percents <- to_wide(percents, desc_name(ct), cohort_name(ct))
    return(percents)
}

#' @export
add_percent_rows <- function(ct, round_to = ROUND_PERCENT_TO, keep_na_vars = F, raw = F) {
    validate_input_add_percent_rows(ct, round_to)

    # Get the rows and add it to the running output table
    percent_rows <- get_percent_rows(ct, long = F, round_to = round_to, keep_na_vars = keep_na_vars, raw = raw)
    ct <- add_rows(ct, percent_rows)

    return(ct)
}

# COUNT AND PERCENT ROWS ####
#' @export
get_count_percent_rows <- function(ct, round_to = ROUND_PERCENT_TO, long = F, long_out_col = COUNT_PERCENT_COL_NAME, keep_na_vars = F) {
    validate_input_get_count_percent_rows(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    counts <- get_count_percent(ct, out_col_name = long_out_col, round_to = round_to, keep_na_vars = keep_na_vars)
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) {
        counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = "0 (0%)")

        # If there are few enough values that there are NAs when pivoting,
        # They are automatically filled with 0 (0%). But if it's also the case that
        # keep_na_vars = T the NA values where description is NA need to just be 0.
        na_rows <- is.na(counts[[desc_name(ct)]])
        cohort_cols <- setdiff(names(counts), desc_name(ct))
        counts[na_rows, cohort_cols][counts[na_rows, cohort_cols] == "0 (0%)"] <- "0"
    }
    return(counts)
}

#' @export
add_count_percent_rows <- function(ct, round_to = ROUND_PERCENT_TO, keep_na_vars = F) {
    validate_input_add_count_percent_rows(ct, round_to)

    # Get the rows and add it to the running output table
    count_percent_rows <- get_count_percent_rows(ct, long = F, round_to = round_to, keep_na_vars = keep_na_vars)
    ct <- add_rows(ct, count_percent_rows)

    return(ct)
}
