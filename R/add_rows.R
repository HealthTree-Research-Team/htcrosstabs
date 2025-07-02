# IMPORTS ####
#' @import assertthat

# CONSTANTS ####
COMP_TOT_DESC <- "Complete / Total"
MEAN_SD_DESC <- "Mean ± SD"
MED_IQR_DESC <- "Med (Q1, Q3)"
DEFAULT_OUT_COL <- "str"

STR_DIF_SUFFIX <- "_auto"

ROUND_PERCENT_TO <- 0

# UTILITIES ####
#' @export
add_rows <- function(ct, rows) {

    # Sanity checks
    assert_that(is.data.frame(rows))
    assert_crosstab(ct)

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

validate_to_wide <- function(long_df, description_col, cohort_col) {
    assert_that(is.data.frame(long_df))
    assert_that(is.character(description_col))
    assert_that(is.character(cohort_col))
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
}

validate_to_long <- function(wide_df, description_col, cohorts_to, values_to) {
    assert_that(is.data.frame(wide_df))
    assert_that(is.character(description_col))
    assert_that(is.character(cohorts_to))
    assert_that(is.character(values_to))
    assert_that(
        description_col %in% names(wide_df),
        msg = "Description column name not found in wide_df"
    )
    assert_that(
        !any(duplicated(c(description_col, cohorts_to, values_to))),
        msg = "description_col, cohorts_to, and values_to must all be unique"
    )
}

get_non_matching <- function(str, dif_than) {
    assert_that(is.character(str), is.character(dif_than))
    while (str %in% dif_than)
        str <- paste0(str, STR_DIF_SUFFIX)
    return(str)
}

validate_cols <- function(ct, long_out_col, wide) {
    if (wide) {
        assert_that(
            !(desc_name(ct) %in% c(cohort_levels(ct), var_name(ct))),
            msg = "If wide = TRUE, desc_col_name must be different than cohort_levels and var_name. Specify a different desc_col_name when creating the original crosstab object."
        )
    } else {
        assert_that(
            !(desc_name(ct) %in% c(cohort_name(ct), var_name(ct), long_out_col)),
            msg = "If wide = FALSE, description column must be different from cohort_col, var_name, and long_out_col. Specify a different long_out_col in this function or a different desc_col_name when creating the original crosstab object."
        )
    }
}

# ROWS ####
#' @export
get_total_row <- function(ct, wide = T, long_out_col = DEFAULT_OUT_COL) {
    assert_crosstab(ct)
    assert_that(is.logical(wide))
    assert_that(is.character(long_out_col))

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
    assert_crosstab(ct)

    total_row <- get_total_row(ct, wide = T)
    ct <- add_rows(ct, total_row)

    return(ct)
}

#' @export
get_mean_sd_row <- function(ct, wide = T, long_out_col = DEFAULT_OUT_COL) {
    assert_crosstab(ct)
    assert_that(is.logical(wide))
    assert_that(is.character(long_out_col))

    # Make sure there is no clash in the intermediate column names
    mean_col <- get_non_matching(MEAN_COL_NAME, cohort_name(ct))
    sd_col <- get_non_matching(SD_COL_NAME, cohort_name(ct))
    if (wide) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))
    validate_cols(ct, long_out_col, wide)

    # Create the string column
    mean_sds <- get_mean(ct, out_col_name = mean_col) |>
        dplyr::full_join(get_sd(ct, out_col_name = sd_col), by = cohort_name(ct))
    mean_sds[[long_out_col]] <- paste0(
        mean_sds[[mean_col]], " ± ", mean_sds[[sd_col]]
    )
    mean_sds <- mean_sds[, c(cohort_name(ct), long_out_col), drop = FALSE]

    # Add description column
    mean_sds[[desc_name(ct)]] <- MEAN_SD_DESC
    mean_sds <- mean_sds[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (wide) mean_sds <- to_wide(mean_sds, desc_name(ct), cohort_name(ct))

    return(mean_sds)
}

#' @export
add_mean_sd_row <- function(ct) {
    assert_crosstab(ct)

    mean_sd_row <- get_mean_sd_row(ct, wide = T)
    ct <- add_rows(ct, mean_sd_row)

    return(ct)
}

#' @export
get_med_iqr_row <- function(ct, wide = T, long_out_col = DEFAULT_OUT_COL) {
    assert_crosstab(ct)
    assert_that(is.logical(wide))
    assert_that(is.character(long_out_col))

    # Make sure there is no clash in the intermediate column names
    med_col <- get_non_matching(MED_COL_NAME, cohort_name(ct))
    q1_col <- get_non_matching(Q1_COL_NAME, cohort_name(ct))
    q3_col <- get_non_matching(Q3_COL_NAME, cohort_name(ct))
    if (wide) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct)))
    validate_cols(ct, long_out_col, wide)

    # Create the string column
    med_iqrs <- get_med(ct, out_col_name = med_col) |>
        dplyr::full_join(get_q1(ct, out_col_name = q1_col), by = cohort_name(ct)) |>
        dplyr::full_join(get_q3(ct, out_col_name = q3_col), by = cohort_name(ct))
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
add_med_iqr_row <- function(ct) {
    assert_crosstab(ct)

    med_iqr_row <- get_med_iqr_row(ct, wide = T)
    ct <- add_rows(ct, med_iqr_row)

    return(ct)
}

#' @export
get_count_rows <- function(ct, wide = T, long_out_col = DEFAULT_OUT_COL, round_to = ROUND_PERCENT_TO) {
    assert_crosstab(ct)
    assert_that(is.logical(wide))
    assert_that(is.character(long_out_col))

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
add_count_rows <- function(ct) {
    assert_crosstab(ct)

    count_rows <- get_count_rows(ct, wide = T)
    ct <- add_rows(ct, count_rows)

    return(ct)
}
