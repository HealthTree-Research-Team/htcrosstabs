# UTILITIES ####

#' Pivot Rows Wider
#'
#' Pivots data in long form to the wide format used in the output table.
#' More speficied in purpose than [tidyr::pivot_wider()] functions.
#'
#' The data passed into `long_df` should contain three columns:
#' * A description column (the leftmost column in the output tables)
#' * A cohort column
#' * A variable column
#'
#' @param long_df The data frame with the data in long form (3 columns: description, cohort, and variable)
#' @param desc_col The name of the description column (the far left in the output tables)
#' @param cohort_col The name of the cohort column
#' @param na_fill The values to replace NAs with when pivoting
#'
#' @returns The data in wide form, formatted for the output table
#' @export
#'
#' @examples
#' # Original long form
#' num_ct <- crosstab(length_by_species, "species")
#' means <- get_mean(num_ct)
#' means
#'
#' # With description column
#' means[["Description"]] <- "Mean Petal Length"
#' means
#'
#' # Wide for output
#' to_wide(means, "Description", "species")
#'
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
    wide_df <- data.frame(wide_df, check.names = FALSE)

    # Fill NA values
    # I KNOW that pivot_wider has a values_fill option, but it requires that the
    # na_fill value match the data type of the rest of the column, whereas doing
    # it this way will cast the data types. It will all be converted to
    # characters anyway in the final output column.
    cohort_cols <- names(wide_df)[names(wide_df) != desc_col]
    wide_df[, cohort_cols][is.na(wide_df[, cohort_cols, drop = FALSE])] <- na_fill

    return(wide_df)
}

#' Pivot Rows Longer
#'
#' Pivots data in wide form (for output) to a longer form easier to extract data
#' from after formatting. More specified in purpose than
#' `tidyr::pivot_longer()`.
#'
#' #' The data passed into `wide_df` should contain:
#' * A description column
#' * Cohort columns
#'
#' @param wide_df The wide dataframe in the typical output format
#' @param description_col The name of the description column
#' @param cohorts_to The name of the cohort column to be created
#' @param values_to The name of the variable column to be created
#'
#' @returns The data in long form, easier for extracting data from
#' @export
#'
#' @examples
#' wide_df <- data.frame(
#'     Description = c("Granny Smith", "Red Delicious"),
#'     All = c(58, 23),
#'     Children = c(12, 11),
#'     Teenagers = c(16, 8),
#'     Adults = c(30, 4)
#' )
#' wide_df
#'
#' to_long(
#'     wide_df = wide_df,
#'     description_col = "Description",
#'     cohorts_to = "Age ranges",
#'     values_to = "Num favorite"
#' )
to_long <- function(wide_df, description_col, cohorts_to, values_to) {
    validate_input_to_long(wide_df, description_col, cohorts_to, values_to)

    # Get the columns to pivot longer
    cohort_cols <- names(wide_df)[names(wide_df) != description_col]

    # Pivot longer
    long_df <- wide_df |>
        tidyr::pivot_longer(
            dplyr::all_of(cohort_cols),
            names_to = cohorts_to,
            values_to = values_to
        )

    # Convert to data frame and re-factorize
    long_df <- data.frame(long_df, check.names = FALSE)
    long_df[[cohorts_to]] <- factor(long_df[[cohorts_to]], levels = cohort_cols)

    return(long_df)
}

#' Add Rows to A Crosstab Object
#'
#' Add a custom or pre-built row (like the `get_*_row()` functions) to the
#' crosstab object and update all relevant attributes.
#'
#' You can insert the rows at a specific place on the table, counting from the
#' top or bottom. The default is inserting at the end of the table, but setting
#' `index_from = "bottom"` will insert it at the beginning (last from the
#' bottom). `index = 3` will insert it before row 3 so it becomes row 3, and
#' `index = 3, index_from = "bottom"` will insert it 3rd from the bottom.
#'
#' @param ct The crosstab to add to
#' @param rows The rows to add to the crosstab
#' @param ind (Optional) The specific row index at which to add the row
#' @param index_from (Optional) Either "top" or "bottom", whether to count the previously specified index from the top or bottom of the table
#' @param section_name Pass a specific table name to tell `index()` that this is a new group of rows
#'
#' @returns The crosstab passed in, but with the extra rows added and attributes updated.
#' @export
#'
#' @examples
#' num_ct <- crosstab(length_by_species, cohort_col_name = "species")
#'
#' complete_total_row <- get_complete_total_row(num_ct)
#' complete_total_row
#'
#' mean_sd_row <- get_mean_sd_row(num_ct)
#' mean_sd_row
#'
#' num_ct <- num_ct |>
#'     add_rows(complete_total_row) |>
#'     add_rows(mean_sd_row)
#' num_ct
#'
add_rows <- function(ct, rows, ind = NULL, index_from = "top", section_name = NULL) {
    validate_input_add_rows(ct, rows, ind, index_from, section_name)

    to_character_cols <- function(df) {
        assert_that(is.data.frame(df))
        dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
    }

    # Calculate index
    if (is.null(ind))
        ind <- nrow(ct) + 1
    ind <- max(1, min(ind, nrow(ct) + 1)) # Clamp the values
    if (index_from == "bottom")
        ind <- nrow(ct) - ind + 2

    char_rows <- to_character_cols(rows)
    orig_rows <- to_character_cols(ct)

    # Insert rows
    if (nrow(orig_rows) == 0) {
        combined <- rows
    } else {
        top <- if (ind > 1) orig_rows[1:(ind - 1), , drop = FALSE] else NULL
        bottom <- if (ind <= nrow(orig_rows)) orig_rows[ind:nrow(orig_rows), , drop = FALSE] else NULL
        combined <- dplyr::bind_rows(top, char_rows, bottom)
    }

    # Check number of columns
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

    # Get what to add to index
    if (is.null(section_name) & is.crosstab(rows)) {
        new_index <- index(rows, long = TRUE)
    } else if (is.null(section_name)) {
        new_index <- rep(var_name(ct), nrow(rows))
    } else {
        new_index <- rep(section_name, nrow(rows))
    }

    # Add the new index values to index
    index(combined) <- insert_at(index(combined, long = TRUE), ind, new_index)

    # Update table_name
    new_table_name <- rep(var_name(ct), nrow(rows))
    table_name(combined) <- insert_at(table_name(combined, long = TRUE), ind, new_table_name)

    # Update table_type
    new_table_type <- rep(get_crosstab_type(ct), nrow(rows))
    table_type(combined) <- insert_at(table_type(combined, long = TRUE), ind, new_table_type)

    # Extend table_id
    cur_id <- table_id(combined, long = TRUE)
    if (is.crosstab(rows)) {
        if (length(cur_id) == 0) {
            new_id <- table_id(rows)
        } else {
            last_id <- cur_id[length(cur_id)]
            new_id <- c(cur_id, (table_id(rows, long = TRUE) + last_id))
        }
    } else {
        if (length(cur_id) == 0) {
            new_id <- rep(1, nrow(rows))
        } else {
            last_id <- cur_id[length(cur_id)]
            new_id <- c(cur_id, rep(last_id, nrow(rows)))
        }
    }
    table_id(combined) <- new_id

    return(combined)
}

# GET ROW DOCUMENTATION ####
#' Get Formatted Output Rows
#'
#' The `get_*_row()` functions all create a data frame formatted for the output
#' table, but separate from the original crosstab object.
#'
#' @section Row Format Components:
#' The rows contain some formatted combination of these variables:
#' * Total: The number of rows in the original data frame
#' * Complete: The number of non-NA rows in the original data frame
#' * Mean: The mean of the numeric data or Likert-like data
#' * SD: The standard deviation of the numeric data or Likert-like data
#' * Med: The median of the numeric data
#' * Q1: The 1st quartile of the numeric data
#' * Q3: The 3rd quartile of the numeric data
#' * Count: The total number of each response in the categorical data
#' * Proportion: The proportion of each response in the categorical data (out of "complete")
#' * Percent: The percent of each response in the categorical data (out of "complete")
#' * ANOVA: The ANOVA p-values and Tukey post-hoc p-values of the numeric data
#' * Chi-Square: The chi-square p-values and adjusted pairwise post-hoc p-values of the numeric data
#' * Rao-Scott: The Rao-Scott adjusted chi-square p-values and adjusted pairwise post-hoc p-values of the numeric data
#'
#' @param ct The crosstab object used to calculate the values
#' @param round_to Numeric - How many decimal places to round the values to
#' @param long Logical - Should the values be printed in long form instead of formatted wide?
#' @param long_out_col, Character - If `long = TRUE`, what should the output column be named?
#' @param keep_na_vars Logical - If `TRUE`, NA variables are treated as values and shown in rows like "count"
#' @param raw Logical - If `TRUE` the function returns the raw numeric values rather than the formatted strings (e.g. 25 instead of "25\%")
#' @param p.adj Logical - Should the p-values be adjusted to account for multiple pairwise tests?
#' @param method Character - The p-value adjustment method (see the man page for [stats::p.adjust()] for possible values)
#' @param cutoff Numeric - The p-value cutoff for significance (usually 0.05)
#'
#' @returns A data frame in the specified format (wide formatted for output unless `long = TRUE`)
#' @name get_formatted_rows
#'
#' @examples
#' num_ct <- crosstab(length_by_species, cohort_col_name = "species")
#'
#' get_mean_sd_row(num_ct)
#' get_med_q1_q3_row(num_ct)
#'
#' get_mean_sd_row(num_ct, long = TRUE)
#' get_med_q1_q3_row(num_ct, long = TRUE)
#'
NULL

# ADD ROW DOCUMENTATION ####
#' Add Formatted Output Rows to Crosstab
#'
#' The `add_*_row()` functions all create a new, formatted row with the
#' specified data and add it to the crosstab object.
#'
#' @inheritSection get_formatted_rows Row Format Components
#'
#' @param ct The crosstab object used to calculate the values
#' @param round_to Numeric - How many decimal places to round the values to
#' @param anova_markers Logical - Append marker characters to indicate significant differences between cohorts
#' @param marker_type Character - What the markers should be; "symbol", "number", or "alphabet"
#' @param superscript Logical - Should the markers be superscripted in the kable output table? (This will work in final Latex output, but will fail in the Viewer)
#' @param keep_na_vars Logical - If `TRUE`, NA variables are treated as values and shown in rows like "count"
#' @param raw Logical - If `TRUE` the function returns the raw numeric values rather than the formatted strings (e.g. 25 instead of "25\%")
#' @param p.adj Logical - Should the p-values be adjusted to account for multiple pairwise tests?
#' @param method Character - The p-value adjustment method (see the man page for [stats::p.adjust()] for possible values)
#' @param cutoff Numeric - The p-value cutoff for significance (usually 0.05)
#'
#' @returns A crosstab object with the specified row(s) added to it
#' @name add_formatted_rows
#'
#' @examples
#' num_ct <- crosstab(length_by_species, cohort_col_name = "species")
#'
#' num_ct # Empty
#'
#' num_ct <- num_ct |>
#'     add_complete_total_row() |>
#'     add_mean_sd_row(anova_markers = TRUE) |>
#'     add_med_q1_q3_row() |>
#'     add_anova_rows()
#'
#' num_ct # Filled with several rows
#'
NULL

# COMPLETE ROW ####
#' @describeIn get_formatted_rows Get the number complete (not NA) for each cohort
#' @export
get_complete_row <- function(ct, long = FALSE, long_out_col = COMP_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the number complete (not NA) for each cohort
#' @export
add_complete_row <- function(ct) {
    validate_input_add_complete_row(ct)

    # Get the row and add it to the running output table
    complete_row <- get_complete_row(ct, long = FALSE)
    ct <- add_rows(ct, complete_row)

    return(ct)
}

# TOTAL ROW ####
#' @describeIn get_formatted_rows Get the number of rows (including NA) for each cohort
#' @export
get_total_row <- function(ct, long = FALSE, long_out_col = TOTAL_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the number of rows (including NA) for each cohort
#' @export
add_total_row <- function(ct) {
    validate_input_add_total_row(ct)

    # Get the row and add it to the running output table
    total_row <- get_total_row(ct, long = FALSE)
    ct <- add_rows(ct, total_row)

    return(ct)
}

# COMPLETE AND TOTAL ROW ####
#' @describeIn get_formatted_rows Get a string formatted as "[complete] / [total]" for each cohort
#' @export
get_complete_total_row <- function(ct, long = FALSE, long_out_col = COMP_TOT_COL_NAME) {
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

#' @describeIn add_formatted_rows Add a string formatted as "[complete] / [total]" for each cohort
#' @export
add_complete_total_row <- function(ct) {
    validate_input_add_complete_total_row(ct)

    # Get the row and add it to the running output table
    comp_total_row <- get_complete_total_row(ct, long = FALSE)
    ct <- add_rows(ct, comp_total_row)
    return(ct)
}

# MEAN ROW ####
#' @describeIn get_formatted_rows Get the mean for each cohort
#' @export
get_mean_row <- function(ct, round_to = MEAN_ROUND_TO, long = FALSE, long_out_col = MEAN_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the mean for each cohort
#' @export
add_mean_row <- function(ct, round_to = MEAN_ROUND_TO, anova_markers = FALSE, marker_type = NULL, superscript = FALSE, cutoff = 0.05) {
    validate_input_add_mean_row(ct, round_to, anova_markers, marker_type, superscript)

    mean_row <- get_mean_row(ct, round_to = round_to, long = FALSE)

    # If anova_markers = TRUE, add the markers before adding the row
    if (anova_markers) {
        marker_list <- get_anova_markers(
            posthoc = ct,
            cohorts = cohort_levels(ct),
            as_str = TRUE,
            marker_type = marker_type,
            superscript = superscript,
            cutoff = cutoff
        )
        for (cohort in names(marker_list)) {
            mean_row[[cohort]] <- paste0(mean_row[[cohort]], marker_list[[cohort]])
        }

        manual_escape(ct) <- manual_escape(ct) | superscript
        ct <- add_anova_marker_footnotes(ct, type = marker_type, cutoff = cutoff)
    }

    ct <- add_rows(ct, mean_row)

    return(ct)
}

# SD ROW ####
#' @describeIn get_formatted_rows Get the standard deviation for each cohort
#' @export
get_sd_row <- function(ct, round_to = SD_ROUND_TO, long = FALSE, long_out_col = SD_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the standard deviation for each cohort
#' @export
add_sd_row <- function(ct, round_to = SD_ROUND_TO) {
    validate_input_add_sd_row(ct, round_to)

    # Get the row and add it to the running output table
    sd_row <- get_sd_row(ct, round_to = round_to, long = FALSE)
    ct <- add_rows(ct, sd_row)

    return(ct)
}

# MEAN AND SD ROW ####
#' @describeIn get_formatted_rows Get a string formatted as "[mean] +/- [sd]" for each cohort
#' @export
get_mean_sd_row <- function(ct, round_to = MEAN_SD_ROUND_TO, long = FALSE, long_out_col = MEAN_SD_COL_NAME) {
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

#' @describeIn add_formatted_rows Add a string formatted as "[mean] +/- [sd]" for each cohort
#' @export
add_mean_sd_row <- function(ct, round_to = MEAN_SD_ROUND_TO, anova_markers = FALSE, marker_type = NULL, superscript = FALSE, cutoff = 0.05) {
    validate_input_add_mean_sd_row(ct, round_to, anova_markers, marker_type, superscript)

    mean_sd_row <- get_mean_sd_row(ct, long = FALSE, round_to = round_to)

    # If anova_markers = TRUE, add the markers before adding the row
    if (anova_markers) {
        marker_list <- get_anova_markers(
            posthoc = ct,
            cohorts = cohort_levels(ct),
            as_str = TRUE,
            marker_type = marker_type,
            superscript = superscript,
            cutoff = cutoff
        )
        for (cohort in names(marker_list)) {
            mean_sd_row[[cohort]] <- paste0(mean_sd_row[[cohort]], marker_list[[cohort]])
        }

        manual_escape(ct) <- manual_escape(ct) | superscript
        ct <- add_anova_marker_footnotes(ct, type = marker_type, cutoff = cutoff)
    }

    ct <- add_rows(ct, mean_sd_row)

    return(ct)
}

# MEDIAN ROW ####
#' @describeIn get_formatted_rows Get the median for each cohort (same as `get_med_row()`)
#' @export
get_median_row <- function(ct, round_to = MEDIAN_ROUND_TO, long = FALSE, long_out_col = MED_COL_NAME) {
    get_med_row(ct, round_to = round_to, long = long, long_out_col = long_out_col)
}

#' @describeIn add_formatted_rows Add the median for each cohort (same as `get_med_row()`)
#' @export
add_median_row <- function(ct, round_to = MEDIAN_ROUND_TO) {
    add_med_row(ct, round_to = round_to)
}

#' @describeIn get_formatted_rows Get the median for each cohort (same as `get_median_row()`)
#' @export
get_med_row <- function(ct, round_to = MEDIAN_ROUND_TO, long = FALSE, long_out_col = MED_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the median for each cohort (same as `get_median_row()`)
#' @export
add_med_row <- function(ct, round_to = MEDIAN_ROUND_TO) {
    validate_input_add_med_row(ct, round_to)

    # Get the row and add it to the running output column
    med_row <- get_med_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, med_row)

    return(ct)
}

# Q1 ROW ####
#' @describeIn get_formatted_rows Get the first quartile for each cohort
#' @export
get_q1_row <- function(ct, round_to = Q1_ROUND_TO, long = FALSE, long_out_col = Q1_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the first quartile for each cohort
#' @export
add_q1_row <- function(ct, round_to = Q1_ROUND_TO) {
    validate_input_add_q1_row(ct, round_to)

    # Get the row and add it to the running output column
    q1_row <- get_q1_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, q1_row)

    return(ct)
}

# Q3 ROW ####
#' @describeIn get_formatted_rows Get the third quartile for each cohort
#' @export
get_q3_row <- function(ct, round_to = Q3_ROUND_TO, long = FALSE, long_out_col = Q3_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the third quartile for each cohort
#' @export
add_q3_row <- function(ct, round_to = Q3_ROUND_TO) {
    validate_input_add_q3_row(ct, round_to)

    # Get the row and add it to the running output column
    q3_row <- get_q3_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, q3_row)

    return(ct)
}

# Q1-Q3 ROW ####
#' @describeIn get_formatted_rows Get a string formatted as "[Q1]--[Q3]" for each cohort
#' @export
get_q1_q3_row <- function(ct, round_to = Q1_Q3_ROUND_TO, long = FALSE, long_out_col = Q1_Q3_COL_NAME) {
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

#' @describeIn add_formatted_rows Add a string formatted as "[Q1]--[Q3]" for each cohort
#' @export
add_q1_q3_row <- function(ct, round_to = Q1_Q3_ROUND_TO) {
    validate_input_add_q1_q3_row(ct, round_to)

    # Get the row and add it to the running output column
    q1_q3_row <- get_q1_q3_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, q1_q3_row)

    return(ct)
}

# IQR ROW ####
#' @describeIn get_formatted_rows Get the IQR (Q3-Q1) for each cohort
#' @export
get_iqr_row <- function(ct, round_to = IQR_ROUND_TO, long = FALSE, long_out_col = IQR_COL_NAME) {
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

#' @describeIn add_formatted_rows Add the IQR (Q3-Q1) for each cohort
#' @export
add_iqr_row <- function(ct, round_to = IQR_ROUND_TO) {
    validate_input_add_iqr_row(ct, round_to)

    # Get the row and add it to the running output column
    iqr_row <- get_iqr_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, iqr_row)

    return(ct)
}

# IQR Q3-Q1 ROW ####
#' @describeIn get_formatted_rows Get a string formatted as "[IQR] ([Q3]-[Q1])" for each cohort
#' @export
get_iqr_q3_q1_row <- function(ct, round_to = IQR_Q3_Q1_ROUND_TO, long = FALSE, long_out_col = IQR_Q3_Q1_COL_NAME) {
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

#' @describeIn add_formatted_rows Add a string formatted as "[IQR] ([Q3]-[Q1])" for each cohort
#' @export
add_iqr_q3_q1_row <- function(ct, round_to = IQR_Q3_Q1_ROUND_TO) {
    validate_input_add_iqr_q3_q1_row(ct, round_to)

    # Get the row and add it to the running output column
    iqr_q3_q1_row <- get_iqr_q3_q1_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, iqr_q3_q1_row)

    return(ct)
}

# MEDIAN AND Q1-Q3 ROW ####
#' @describeIn get_formatted_rows Get a string formatted as "[median] ([Q1], [Q3])" for each cohort
#' @export
get_med_q1_q3_row <- function(ct, round_to = MED_Q1_Q3_ROUND_TO, long = FALSE, long_out_col = MED_Q1_Q3_COL_NAME) {
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

#' @describeIn add_formatted_rows Add a string formatted as "[median] ([Q1], [Q3])" for each cohort
#' @export
add_med_q1_q3_row <- function(ct, round_to = MED_Q1_Q3_ROUND_TO) {
    validate_input_add_med_q1_q3_row(ct, round_to)

    # Get the row and add it to the running output column
    med_q1_q3_row <- get_med_q1_q3_row(ct, long = FALSE, round_to = round_to)
    ct <- add_rows(ct, med_q1_q3_row)

    return(ct)
}

# COUNT ROWS ####
#' @describeIn get_formatted_rows Get the counts for each response and cohort
#' @export
get_count_rows <- function(ct, long = FALSE, long_out_col = COUNT_COL_NAME, keep_na_vars = FALSE) {
    validate_input_get_count_rows(ct, long, long_out_col)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    counts <- get_count(ct, out_col_name = long_out_col, keep_na_vars = keep_na_vars)
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = 0)
    return(counts)
}

#' @describeIn add_formatted_rows Add the counts for each response and cohort
#' @export
add_count_rows <- function(ct, keep_na_vars = FALSE) {
    validate_input_add_count_rows(ct)

    count_rows <- get_count_rows(ct, long = FALSE, keep_na_vars = keep_na_vars)
    ct <- add_rows(ct, count_rows)

    return(ct)
}

# PROPORTION ROWS ####
#' @describeIn get_formatted_rows Get the proportion of responses (out of complete) for each response and cohort (same as `get_prop_rows()`)
#' @export
get_proportion_rows <- function(ct, round_to = PROP_ROUND_TO, long = FALSE, long_out_col = PROP_COL_NAME, keep_na_vars = FALSE) {
    get_prop_rows(ct, round_to = round_to, long = long, long_out_col = long_out_col, keep_na_vars = keep_na_vars)
}

#' @describeIn add_formatted_rows Add the proportion of responses (out of complete) for each response and cohort (same as `get_prop_rows()`)
#' @export
add_proportion_rows <- function(ct, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    add_prop_rows(ct, round_to = round_to, keep_na_vars = keep_na_vars)
}

#' @describeIn get_formatted_rows Get the proportion of responses (out of complete) for each response and cohort (same as `get_proportion_rows()`)
#' @export
get_prop_rows <- function(ct, round_to = PROP_ROUND_TO, long = FALSE, long_out_col = PROP_COL_NAME, keep_na_vars = FALSE) {
    validate_input_get_prop_rows(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    props <- get_prop(ct, out_col_name = long_out_col, round_to = round_to, keep_na_vars = keep_na_vars)
    names(props)[names(props) == var_name(ct)] <- desc_name(ct)
    props <- props[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) props <- to_wide(props, desc_name(ct), cohort_name(ct))
    return(props)
}

#' @describeIn add_formatted_rows Add the proportion of responses (out of complete) for each response and cohort (same as `get_proportion_rows()`)
#' @export
add_prop_rows <- function(ct, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    validate_input_add_prop_rows(ct, round_to)

    prop_rows <- get_prop_rows(ct, long = FALSE, round_to = round_to, keep_na_vars = keep_na_vars)
    ct <- add_rows(ct, prop_rows)

    return(ct)
}

# COUNT AND PROPORTION ROWS ####
#' @describeIn get_formatted_rows Get a string formatted as "[count] ([prop])" for each response and cohort
#' @export
get_count_prop_rows <- function(ct, round_to = COUNT_PROP_ROUND_TO, long = FALSE, long_out_col = COUNT_PROP_COL_NAME, keep_na_vars = FALSE) {
    validate_input_get_count_prop_rows(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)


    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    counts <- get_count_prop(ct, out_col_name = long_out_col, round_to = round_to, keep_na_vars = keep_na_vars)
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = "0 (0)")
    return(counts)
}

#' @describeIn add_formatted_rows Add a string formatted as "[count] ([prop])" for each response and cohort
#' @export
add_count_prop_rows <- function(ct, round_to = COUNT_PROP_ROUND_TO, keep_na_vars = FALSE) {
    validate_input_add_count_prop_rows(ct, round_to)

    count_percent_rows <- get_count_percent_rows(ct, long = FALSE, round_to = round_to, keep_na_vars = keep_na_vars)
    ct <- add_rows(ct, count_percent_rows)

    return(ct)
}

# PERCENT ROWS ####
#' @describeIn get_formatted_rows Get the percent (out of complete) for each response and cohort
#' @export
get_percent_rows <- function(ct, round_to = PERCENT_ROUND_TO, long = FALSE, long_out_col = PERCENT_COL_NAME, keep_na_vars = FALSE, raw = FALSE) {
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

#' @describeIn add_formatted_rows Add the percent (out of complete) for each response and cohort
#' @export
add_percent_rows <- function(ct, round_to = PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    validate_input_add_percent_rows(ct, round_to)

    percent_rows <- get_percent_rows(ct, long = FALSE, round_to = round_to, keep_na_vars = keep_na_vars, raw = raw)
    ct <- add_rows(ct, percent_rows)

    return(ct)
}

# COUNT AND PERCENT ROWS ####
#' @describeIn get_formatted_rows Get a string formatted as "[count] ([percent]\%)" for each response and cohort
#' @export
get_count_percent_rows <- function(ct, round_to = COUNT_PERCENT_ROUND_TO, long = FALSE, long_out_col = COUNT_PERCENT_COL_NAME, keep_na_vars = FALSE) {
    validate_input_get_count_percent_rows(ct, long, long_out_col, round_to)
    validate_input_col_names(ct, long_out_col, long)

    # If there is a clash in the long_out_col, change it
    if (!long) long_out_col <- get_non_matching(long_out_col, c(desc_name(ct), cohort_name(ct), var_name(ct)))

    # Get values and rename variable column to description column
    counts <- get_count_percent(ct, out_col_name = long_out_col, round_to = round_to, keep_na_vars = keep_na_vars)
    names(counts)[names(counts) == var_name(ct)] <- desc_name(ct)
    counts <- counts[, c(desc_name(ct), cohort_name(ct), long_out_col), drop = FALSE]

    if (!long) counts <- to_wide(counts, desc_name(ct), cohort_name(ct), na_fill = "0 (0%)")
    return(counts)
}

#' @describeIn add_formatted_rows Add a string formatted as "[count] ([percent]\%)" for each response and cohort
#' @export
add_count_percent_rows <- function(ct, round_to = COUNT_PERCENT_ROUND_TO, keep_na_vars = FALSE) {
    validate_input_add_count_percent_rows(ct, round_to)

    count_percent_rows <- get_count_percent_rows(ct, long = FALSE, round_to = round_to, keep_na_vars = keep_na_vars)
    ct <- add_rows(ct, count_percent_rows)

    return(ct)
}

# ANOVA ROWS ####
#' @describeIn get_formatted_rows Get the rows formatted with ANOVA p-value and Tukey post-hoc (if applicable)
#' @export
get_anova_rows <- function(ct, cutoff = 0.05, round_to = 3) {
    UseMethod("get_anova_rows", ct)
}

#' @noRd
#' @export
get_anova_rows.crosstab <- function(ct, cutoff = 0.05, round_to = 3) {
    get_anova_rows(data_table(ct), cutoff = cutoff, round_to = round_to)
}

#' @noRd
#' @export
get_anova_rows.crosstab_data <- function(ct, cutoff = 0.05, round_to = 3) {
    validate_input_get_anova_rows(ct, cutoff, round_to)

    # Create skeleton for new rows
    new_rows <- create_stat_row_skeleton(ct)

    # Fill the matrix with p-values
    new_rows <- fill_stat_row_skeleton(
        new_rows = new_rows,
        data = ct,
        posthoc = get_tukey_posthoc(ct),
        overall_p_value = get_anova_p_value(ct),
        cutoff = cutoff,
        round_to = round_to
    )

    return(new_rows)
}

#' @describeIn add_formatted_rows Add the rows formatted with ANOVA p-value and Tukey post-hoc (if applicable)
#' @export
add_anova_rows <- function(ct, cutoff = 0.05, round_to = 3) {
    validate_input_add_anova_rows(ct, cutoff, round_to)

    anova_rows <- get_anova_rows(ct, cutoff = cutoff, round_to = round_to)

    ct <- add_anova_row_footnotes(ct, cutoff = cutoff)
    ct <- add_rows(ct, anova_rows, section_name = "ANOVA Results")
    return(ct)
}

# CHI-SQUARE ROWS ####
#' @describeIn get_formatted_rows Get the rows formatted with chi-square p-value and pairwise post-hoc (if applicable)
#' @export
get_chisq_rows <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    UseMethod("get_chisq_rows", ct)
}

#' @noRd
#' @export
get_chisq_rows.crosstab <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    get_chisq_rows(data_table(ct), p.adj = p.adj, method = method, cutoff = cutoff, round_to = round_to)
}

#' @noRd
#' @export
get_chisq_rows.crosstab_data_multi <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    get_rao_scott_rows(ct, p.adj = p.adj, method = method, cutoff = cutoff, round_to = round_to)
}

#' @noRd
#' @export
get_chisq_rows.crosstab_data <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    validate_input_get_chisq_rows(ct, p.adj, method, cutoff, round_to)

    new_rows <- create_stat_row_skeleton(ct)
    new_rows <- fill_stat_row_skeleton(
        new_rows = new_rows,
        data = ct,
        posthoc = get_chisq_posthoc(ct, p.adj = p.adj, method = method),
        overall_p_value = get_chisq_p_value(ct),
        cutoff = cutoff,
        round_to = round_to
    )
    return(new_rows)
}

#' @describeIn add_formatted_rows Add the rows formatted with chi-square p-value and pairwise post-hoc (if applicable)
#' @export
add_chisq_rows <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    validate_input_add_chisq_rows(ct, p.adj, method, cutoff, round_to)

    if (is.crosstab.multi(ct))
        return(add_rao_scott_rows(ct, p.adj = p.adj, method = method, cutoff = cutoff, round_to = round_to))

    chisq_rows <- get_chisq_rows(
        ct,
        p.adj = p.adj,
        method = method,
        cutoff = cutoff,
        round_to = round_to
    )

    ct <- add_chisq_row_footnotes(ct, p.adj = p.adj, method = method, cutoff = cutoff)

    ct <- add_rows(ct, chisq_rows, section_name = "Pearson Chi-Square Results")
    return(ct)
}

# RAO-SCOTT ROWS ####
#' @describeIn get_formatted_rows Get the rows formatted with Rao-Scott chi-square p-value and pairwise posthoc (if applicable)
#' @export
get_rao_scott_rows <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    UseMethod("get_rao_scott_rows", ct)
}

#' @noRd
#' @export
get_rao_scott_rows.crosstab <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    get_rao_scott_rows(data_table(ct), p.adj = p.adj, method = method, cutoff = cutoff, round_to = round_to)
}

#' @noRd
#' @export
get_rao_scott_rows.crosstab_data <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    validate_input_get_rao_scott_rows(ct, p.adj, method, cutoff, round_to)

    new_rows <- create_stat_row_skeleton(ct)
    new_rows <- fill_stat_row_skeleton(
        new_rows = new_rows,
        data = ct,
        posthoc = get_rao_scott_posthoc(ct, p.adj = p.adj, method = method),
        overall_p_value = get_rao_scott_p_value(ct),
        cutoff = cutoff,
        round_to = round_to
    )
    return(new_rows)
}

#' @describeIn add_formatted_rows Add the rows formatted with Rao-Scott chi-square p-value and pairwise posthoc (if applicable)
#' @export
add_rao_scott_rows <- function(ct, p.adj = TRUE, method = "BH", cutoff = 0.05, round_to = 3) {
    validate_input_add_rao_scott_rows(ct, p.adj, method, cutoff, round_to)

    rao_scott_rows <- get_rao_scott_rows(
        ct,
        p.adj = p.adj,
        method = method,
        cutoff = cutoff,
        round_to = round_to
    )

    ct <- add_rao_scott_row_footnotes(ct, p.adj = p.adj, method = method, cutoff = cutoff)

    ct <- add_rows(ct, rao_scott_rows, section_name = "Rao-Scott Chi-Square Results")
    return(ct)
}
