#' @export
add_categorical_table <- function(
        ct,
        chisq = T,
        keep_na_vars = F,
        round_percent_to = PERCENT_ROUND_TO,
        p.adj = T,
        method = "BH",
        cutoff = 0.05,
        round_p_val_to = 3
) {
    validate_input_add_categorical_table(ct, chisq, keep_na_vars, round_percent_to, p.adj, method, cutoff, round_p_val_to)
    if (!is.crosstab.grouped(ct)) chisq <- F

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

#' @export
add_numeric_table <- function(
        ct,
        anova = T,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_med_q1_q3_to = MED_Q1_Q3_ROUND_TO,
        round_p_val_to = 3,
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = F
) {
    validate_input_add_numeric_table(ct, anova, round_mean_sd_to, round_med_q1_q3_to, round_p_val_to, cutoff, anova_format)
    if (!is.crosstab.grouped(ct))
        anova <- F

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

#' @export
add_likert_table <- function(
        ct,
        anova = T,
        chisq = T,
        keep_na_vars = F,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_percent_to = PERCENT_ROUND_TO,
        round_p_val_to = 3,
        p.adj = T,
        method = "BH",
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = F
) {
    validate_input_add_likert_table(ct, anova, chisq, keep_na_vars, round_mean_sd_to, round_percent_to, round_p_val_to, p.adj, method, cutoff, anova_format)
    if (!is.crosstab.grouped(ct)) {
        anova <- F
        chisq <- F
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

#' @export
add_default_table <- function(
        ct,
        anova = T,
        chisq = T,
        keep_na_vars = F,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_med_q1_q3_to = MED_Q1_Q3_ROUND_TO,
        round_percent_to = PERCENT_ROUND_TO,
        round_p_val_to = 3,
        p.adj = T,
        method = "BH",
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = F
) {
    validate_input_add_default_table(ct, anova, chisq, keep_na_vars, round_mean_sd_to, round_med_q1_q3_to, round_percent_to, round_p_val_to, p.adj, method, cutoff, anova_format)

    if (!is.crosstab.grouped(ct)) {
        anova <- F
        chisq <- F
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

#' @export
default_stacked_crosstab <- function(
        df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description",
        anova = T,
        chisq = T,
        keep_na_vars = F,
        round_mean_sd_to = MEAN_SD_ROUND_TO,
        round_med_q1_q3_to = MED_Q1_Q3_ROUND_TO,
        round_percent_to = PERCENT_ROUND_TO,
        round_p_val_to = 3,
        p.adj = T,
        method = "BH",
        cutoff = 0.05,
        anova_format = "row",
        marker_type = NULL,
        superscript = F
) {
    validate_input_default_stacked_crosstab(df, cohort_col_name, var_map)

    if (is.null(cohort_col_name)) {
        anova = F
        chisq = F
    }

    cols <- names(df)
    if (!is.null(cohort_col_name))
        cols <- cols[cols != cohort_col_name]

    if (!is.list(var_map)) {
        matching_col_indices <- sapply(df[, cols, drop = F], function(col) all(col %in% c(names(var_map), NA)))
        matching_col_names <- cols[matching_col_indices]
        var_map <- rep(list(var_map), length(matching_col_names))
        names(var_map) <- matching_col_names
    }

    create_default_ct <- function(col) {
        filtered_df <- df[, c(col, cohort_col_name), drop = F]

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
