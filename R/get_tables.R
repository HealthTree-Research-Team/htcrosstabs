#' Add Pre-Built Tables
#'
#' The `add_*_table()` functions add pre-built sets of rows for different types
#' of data (e.g. numeric, Likert-like, and categorical - including multi-response).
#' `add_default_table()` automatically detects what kind of table to add based
#' on the data type.
#'
#' @section Categorical Table:
#' * "Complete / Total" row
#' * Count rows
#' * Chi-square rows (unless chisq = FALSE)
#'
#' @section Numeric Table:
#' * "Complete / Total" row
#' * "Mean +/- SD" row
#' * "Median (Q1, Q3)" row
#' * ANOVA rows (unless anova = FALSE)
#'
#' @section Likert Table:
#' * "Complete / Total" row
#' * "Mean +/- SD" row
#' * Count rows
#' * ANOVA rows (unless anova = FALSE)
#' * Chi-square rows (unless chisq = FALSE)
#'
#' @param ct The crosstab object to add to
#' @param anova Logical - Should ANOVA tests be performed and info added?
#' @param chisq Logical - Should chi-square tests be performed and info added?
#' @param round_mean_sd_to Numeric - How many digits to round the mean and sd to
#' @param round_med_q1_q3_to Numeric - How many digits to round the median, q1, and q3 to
#' @param round_percent_to Numeric - How many digits to round the percent to
#' @param round_p_val_to Numeric - How many digits to round the p-value to
#' @param keep_na_vars Logical - Should NA variables be kept in the output of "count" rows?
#' @param p.adj Logical - Should the chi-square post-hoc p-values be adjusted to account for multiple tests?
#' @param method Character - The p-value adjustment method (see the man page for [stats::p.adjust()] for possible values)
#' @param cutoff Numeric - The p-value cutoff for significance (usually 0.05)
#' @param anova_format Character - "row" for ANOVA data as rows, "marker" for ANOVA data as markers appended to mean and sd data, and c("row", "marker") for both
#' @param marker_type Character - What the markers should be; "symbol", "number", or "alphabet"
#' @param superscript Logical - Should the markers be superscripted in the kable output table? (This will work in final Latex output, but will fail in the Viewer)
#'
#' @returns The crosstab object with the specified tables added to it
#' @name add_formatted_table
#'
#' @examples
#' # Numeric Table
#' num_ct <- crosstab(
#'     length_by_species,
#'     cohort_col_name = "species"
#' )
#'
#' num_ct <- num_ct |>
#'     add_numeric_table()
#'
#' num_ct
#'
#' # Likert table
#' licorice_map <- c("likes" = 1, "neither" = 0, "dislikes" = -1)
#' likert_ct <- crosstab(
#'     licorice_by_region,
#'     cohort_col_name = "region",
#'     var_map = licorice_map
#' )
#'
#' likert_ct <- likert_ct |>
#'     add_likert_table()
#'
#' likert_ct
NULL

#' @describeIn add_formatted_table Add the default table for categorical data (including multi-response)
#' @export
add_categorical_table <- function(
        ct,
        chisq = TRUE,
        keep_na_vars = FALSE,
        round_percent_to = PERCENT_ROUND_TO,
        p.adj = TRUE,
        method = "BH",
        cutoff = 0.05,
        round_p_val_to = 3
) {
    validate_input_add_categorical_table(ct, chisq, keep_na_vars, round_percent_to, p.adj, method, cutoff, round_p_val_to)
    if (!is.crosstab.grouped(ct)) chisq <- FALSE

    result <- ct |>
        add_complete_total_row() |>
        add_count_percent_rows(round_to = round_percent_to, keep_na_vars = keep_na_vars)

    if (chisq) {
        result <- result |>
            add_chisq_rows(
                p.adj = p.adj,
                method = method,
                cutoff = cutoff,
                round_to = round_p_val_to
            )
    }

    return(result)
}

#' @describeIn add_formatted_table Add the default table for numeric data
#' @export
add_numeric_table <- function(
        ct,
        anova = TRUE,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_med_q1_q3_to = MED_Q1_Q3_ROUND_TO,
        round_p_val_to = 3,
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = FALSE
) {
    validate_input_add_numeric_table(ct, anova, round_mean_sd_to, round_med_q1_q3_to, round_p_val_to, cutoff, anova_format)
    if (!is.crosstab.grouped(ct))
        anova <- FALSE

    result <- ct |>
        add_complete_total_row() |>
        add_mean_sd_row(
            round_to = round_mean_sd_to,
            anova_markers = anova & ("marker" %in% anova_format),
            marker_type = marker_type,
            superscript = superscript,
            cutoff = cutoff
        ) |>
        add_med_q1_q3_row(round_to = round_med_q1_q3_to)

    if (anova & ("row" %in% anova_format))
        result <- add_anova_rows(
            result,
            cutoff = cutoff,
            round_to = round_p_val_to
        )

    return(result)
}

#' @describeIn add_formatted_table Add the default table for Likert-like data
#' @export
add_likert_table <- function(
        ct,
        anova = TRUE,
        chisq = TRUE,
        keep_na_vars = FALSE,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_percent_to = PERCENT_ROUND_TO,
        round_p_val_to = 3,
        p.adj = TRUE,
        method = "BH",
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = FALSE
) {
    validate_input_add_likert_table(ct, anova, chisq, keep_na_vars, round_mean_sd_to, round_percent_to, round_p_val_to, p.adj, method, cutoff, anova_format)
    if (!is.crosstab.grouped(ct)) {
        anova <- FALSE
        chisq <- FALSE
    }

    result <- ct |>
        add_complete_total_row() |>
        add_mean_sd_row(
            round_to = round_mean_sd_to,
            anova_markers = anova & ("marker" %in% anova_format),
            marker_type = marker_type,
            superscript = superscript,
            cutoff = cutoff
        ) |>
        add_count_percent_rows(
            round_to = round_percent_to,
            keep_na_vars = keep_na_vars
        )

    # Add ANOVA row if applicable
    if (anova & ("row" %in% anova_format))
        result <- add_anova_rows(
            result,
            cutoff = cutoff,
            round_to = round_p_val_to
        )

    # Add chi-square if applicable
    if (chisq)
        result <- add_chisq_rows(
            result,
            p.adj = p.adj,
            method = method,
            cutoff = cutoff,
            round_to = round_p_val_to
        )

    return(result)
}

#' @describeIn add_formatted_table Add the proper table based on data type
#' @export
add_default_table <- function(
        ct,
        anova = TRUE,
        chisq = TRUE,
        keep_na_vars = FALSE,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_med_q1_q3_to = MED_Q1_Q3_ROUND_TO,
        round_percent_to = PERCENT_ROUND_TO,
        round_p_val_to = 3,
        p.adj = TRUE,
        method = "BH",
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = FALSE
) {
    validate_input_add_default_table(ct, anova, chisq, keep_na_vars, round_mean_sd_to, round_med_q1_q3_to, round_percent_to, round_p_val_to, p.adj, method, cutoff, anova_format)

    if (!is.crosstab.grouped(ct)) {
        anova <- FALSE
        chisq <- FALSE
    }

    if (is.crosstab.numeric(ct)) {
        add_numeric_table(
            ct,
            anova = anova,
            round_mean_sd_to = round_mean_sd_to,
            round_med_q1_q3_to = round_med_q1_q3_to,
            round_p_val_to = round_p_val_to,
            cutoff = cutoff,
            anova_format = anova_format,
            marker_type = marker_type,
            superscript = superscript
        )
    } else if (is.crosstab.likert(ct)) {
        add_likert_table(
            ct,
            anova = anova,
            chisq = chisq,
            keep_na_vars = keep_na_vars,
            round_mean_sd_to = round_mean_sd_to,
            round_percent_to = round_percent_to,
            round_p_val_to = round_p_val_to,
            p.adj = p.adj,
            method = method,
            cutoff = cutoff,
            anova_format = anova_format,
            marker_type = marker_type,
            superscript = superscript
        )
    } else {
        add_categorical_table(
            ct,
            chisq = chisq,
            keep_na_vars = keep_na_vars,
            round_percent_to = round_percent_to,
            p.adj = p.adj,
            method = method,
            cutoff = cutoff,
            round_p_val_to = round_p_val_to
        )
    }
}

#' Create Stacked Tables From Multiple Data Columns
#'
#' While [crosstab()] handles the base case of one variable column (and one
#' cohort column if applicable), `crosstab_stacked()` iterates over each
#' variable column to create a stacked crosstab object with multiple combined
#' tables. It uses the same arguments that [crosstab()] does to determine type.
#' If you have 2 likert columns with different var_maps, you can make a list
#' with both and pass that in (the names of the list should match the columns
#' they correspond to).
#'
#' @param df The original data frame with the data to be analyzed
#' @param cohort_col_name The name of the cohort column, so it can be paired with each sub-crosstab
#' @param var_map A named numeric vector (or list of vectors) for Likert-like data mapping categorical values to numeric values
#' @param combined_cohort_name The name for the combined or "All" column for grouped data
#' @param desc_col_name The name for the leftmost "description" column in the output table
#' @param anova Logical - Should ANOVA tests be performed and info added?
#' @param chisq Logical - Should chi-square tests be performed and info added?
#' @param round_mean_sd_to Numeric - How many digits to round the mean and sd to
#' @param round_med_q1_q3_to Numeric - How many digits to round the median, q1, and q3 to
#' @param round_percent_to Numeric - How many digits to round the percent to
#' @param round_p_val_to Numeric - How many digits to round the p-value to
#' @param keep_na_vars Logical - Should NA variables be kept in the output of "count" rows?
#' @param p.adj Logical - Should the chi-square post-hoc p-values be adjusted to account for multiple tests?
#' @param method Character - The p-value adjustment method (see the man page for [stats::p.adjust()] for possible values)
#' @param cutoff Numeric - The p-value cutoff for significance (usually 0.05)
#' @param anova_format Character - "row" for ANOVA data as rows, "marker" for ANOVA data as markers appended to mean and sd data, and c("row", "marker") for both
#' @param marker_type Character - What the markers should be; "symbol", "number", or "alphabet"
#' @param superscript Logical - Should the markers be superscripted in the kable output table? (This will work in final Latex output, but will fail in the Viewer)
#'
#' @returns The stacked crosstab object
#' @export
#'
#' @examples
#' # All numeric data
#' crosstab_stacked(
#'     iris,
#'     cohort_col_name = "Species"
#' )
#'
#' # Combining different types of data
#' uni_perception_map <- c(
#'     "overwhelmingly positive" = 2,
#'     "somewhat positive" = 1,
#'     "neutral" = 0,
#'     "somewhat negative" = -1,
#'     "overwhelmingly negative" = -2
#' )
#'
#' prof_support_map <- c(
#'     "very supportive" = 4,
#'     "somewhat supportive" = 3,
#'     "apathetic" = 2,
#'     "somewhat demeaning" = 1,
#'     "very demeaning" = 0
#' )
#'
#' likert_maps <- list(
#'     uni_perception = uni_perception_map,
#'     prof_support = prof_support_map
#' )
#'
#' crosstab_stacked(
#'     students,
#'     cohort_col_name = "university",
#'     var_map = likert_maps,
#'     anova = FALSE,
#'     chisq = FALSE
#' )
#'
crosstab_stacked <- function(
        df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description",
        anova = TRUE,
        chisq = TRUE,
        keep_na_vars = FALSE,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_med_q1_q3_to = MED_Q1_Q3_ROUND_TO,
        round_percent_to = PERCENT_ROUND_TO,
        round_p_val_to = 3,
        p.adj = TRUE,
        method = "BH",
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = FALSE
) {
    validate_input_crosstab_stacked(df, cohort_col_name, var_map)

    if (is.null(cohort_col_name)) {
        anova = FALSE
        chisq = FALSE
    }

    cols <- names(df)
    if (!is.null(cohort_col_name))
        cols <- cols[cols != cohort_col_name]

    if (!is.list(var_map)) {
        matching_col_indices <- sapply(df[, cols, drop = FALSE], function(col) all(col %in% c(names(var_map), NA)))
        matching_col_names <- cols[matching_col_indices]
        var_map <- rep(list(var_map), length(matching_col_names))
        names(var_map) <- matching_col_names
    }

    create_default_ct <- function(col) {
        filtered_df <- df[, c(col, cohort_col_name), drop = FALSE]

        new_ct <- crosstab(
            df = filtered_df,
            cohort_col_name = cohort_col_name,
            var_map = var_map[[col]],
            combined_cohort_name = combined_cohort_name,
            desc_col_name = desc_col_name
        )

        add_default_table(
            ct = new_ct,
            anova = anova,
            chisq = chisq,
            keep_na_vars = keep_na_vars,
            round_mean_sd_to = round_mean_sd_to,
            round_med_q1_q3_to = round_med_q1_q3_to,
            round_percent_to = round_percent_to,
            round_p_val_to = round_p_val_to,
            p.adj = p.adj,
            method = method,
            cutoff = cutoff,
            anova_format = anova_format,
            marker_type = marker_type,
            superscript = superscript
        )
    }

    cts <- lapply(cols, create_default_ct)
    cts <- do.call(stack_crosstabs, cts)
    return(cts)
}
