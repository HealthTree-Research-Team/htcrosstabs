validate_input_add_categorical_table <- function(ct, chisq, keep_na_vars, round_percent_to, p.adj, method, cutoff, round_p_val_to) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(chisq))
    assert_that(is.logical(keep_na_vars))
    assert_that(is.numeric(round_percent_to))
    assert_that(is.logical(p.adj))
    assert_that(is.character(method))
    assert_that(is.numeric(cutoff))
    assert_that(cutoff > 0)
    assert_that(is.numeric(round_p_val_to))
}

validate_input_add_numeric_table <- function(ct, anova, round_mean_sd_to, round_med_q1_q3_to, round_p_val_to, cutoff, anova_format) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(anova))
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_med_q1_q3_to))
    assert_that(is.numeric(round_p_val_to))
    assert_that(is.numeric(cutoff))
    assert_that(cutoff > 0)
    assert_that(is.character(anova_format))
    if (anova) {
        assert_that(
            all(anova_format %in% c("row", "marker")),
            msg = "anova_format must be either \"row\", \"marker\", or a combination like c(\"row\", \"marker\")"
        )
    }
}

validate_input_add_likert_table <- function(ct, anova, chisq, keep_na_vars, round_mean_sd_to, round_percent_to, round_p_val_to, p.adj, method, cutoff, anova_format) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(anova))
    assert_that(is.logical(chisq))
    assert_that(is.logical(keep_na_vars))
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_percent_to))
    assert_that(is.numeric(round_p_val_to))
    assert_that(is.logical(p.adj))
    assert_that(is.character(method))
    assert_that(is.numeric(cutoff))
    assert_that(cutoff > 0)
}

validate_input_add_default_table <- function(ct, anova, chisq, keep_na_vars, round_mean_sd_to, round_med_q1_q3_to, round_percent_to, round_p_val_to, p.adj, method, cutoff, anova_format) {
    assert_crosstab(ct, strict = T)
    assert_that(is.logical(anova))
    assert_that(is.logical(chisq))
    assert_that(is.logical(keep_na_vars))
    assert_that(is.numeric(round_mean_sd_to))
    assert_that(is.numeric(round_med_q1_q3_to))
    assert_that(is.numeric(round_percent_to))
    assert_that(is.numeric(round_p_val_to))
    assert_that(is.logical(p.adj))
    assert_that(is.character(method))
    assert_that(is.numeric(cutoff))
    assert_that(is.character(anova_format))
    if (anova) {
        assert_that(
            all(anova_format %in% c("row", "marker")),
            msg = "anova_format must be either \"row\", \"marker\", or a combination like c(\"row\", \"marker\")"
        )
    }
}

validate_input_default_stacked_crosstab <- function(df, cohort_col_name, var_map) {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name)) {
        assert_that(is.character(cohort_col_name))
        assert_that(cohort_col_name %in% names(df), msg = sprintf(
            "Cohort column name \"%s\" not found in provided data frame",
            cohort_col_name
        ))
    }

    if (!is.null(var_map)) {
        if (is.list(var_map)) {
            sapply(var_map, function(map) {
                assert_that(
                    is.numeric(map), !is.null(names(map)),
                    msg = "If var_map is a list, all elements must be named numeric vectors"
                )
            })

            var_col_names <- setdiff(names(df), cohort_col_name)

            extra_names <- names(var_map)[!(names(var_map) %in% var_col_names)]

            if (length(extra_names) != 0) {
                warning(sprintf(
                    "The names \"%s\" are present in var_map, but not df",
                    paste(extra_names, collapse = "\", \"")
                ))
            }
        } else {
            assert_that(
                is.numeric(var_map), !is.null(names(var_map)),
                msg = "var_map must be either a named numeric vector or a list of named numeric vectors"
            )

            if (!any(sapply(df, function(col) {
                all(col %in% c(names(var_map), NA))
            }))) {
                warning(sprintf(
                    "var_map provided, but found no columns to map it to; every column contains unmapped values."
                ))
            }
        }
    }

    assert_that(
        length(setdiff(names(df), cohort_col_name)) > 0,
        msg = "df must have at least 1 column aside from the cohort column (if applicable)"
    )
}
