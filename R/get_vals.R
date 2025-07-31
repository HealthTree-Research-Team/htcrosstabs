# UTILITIES ####
#' Join Multiple Value Results
#'
#' Provides an easy interface to join the multiple variables you need into one
#' data frame. This is essentially [dplyr::full_join()] but automatically
#' detects which columns to join by and allows you to pass in as many tables as
#' you want to join.
#'
#' @param ... As many data frames as you wish to join, separated by commas
#'
#' @returns The resulting full-joined data frame
#' @export
#'
#' @examples
#' test_ct <- crosstab(length_by_species, "species")
#'
#' # One cohort and one variable column each
#' join_val(
#'     get_mean(test_ct),
#'     get_sd(test_ct),
#'     get_mean_sd(test_ct)
#' )
#'
#' # Tables with multiple columns
#' join_val(
#'     get_complete_total(test_ct, raw = TRUE),
#'     get_med_q1_q3(test_ct, raw = TRUE)
#' )
#'
join_val <- function(...) {
    dfs <- list(...)

    # Check that all inputs are data frames
    assert_that(
        all(vapply(dfs, is.data.frame, logical(1))),
        msg = "All inputs to join_val() must be data frames"
    )

    # Reduce over full_join, automatically finding common columns
    Reduce(function(x, y) {
        common_cols <- intersect(names(x), names(y))
        assert_that(length(common_cols) > 0, msg = "No common columns to join on between some data frames.")
        dplyr::full_join(x, y, by = common_cols)
    }, dfs)
}

# GET VALUE DOCUMENTATION ####
#' Get Values Calculated From Crosstab
#'
#' The `get_*()` functions return a data frame with the specified data
#' calculated from the provided crosstab, casted if necessary. These are the
#' base values that the other functions use to produce their formatted output.
#'
#' @param ct_data The crosstab or crosstab_data object
#' @param out_col_name Character - A custom name for the variable column in the output
#' @param round_to Numeric - The number of digits to round the specified value to
#' @param keep_na_vars Logical - Whether or not NA values should be included in variables like count
#' @param raw Logical - If `TRUE` the data frame returned contains the raw values rather than one formatted column
#'
#' @returns A data frame with the specified data as well as other columns to differentiate
#' @name get_values
#'
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @examples
#' test_ct <- crosstab(length_by_species, "species")
#'
#' # Base values
#' get_complete(test_ct)
#' get_mean(test_ct)
#' get_sd(test_ct)
#'
#' # Formatted column
#' get_complete_total(test_ct)
#' get_med_q1_q3(test_ct)
#'
#' # Raw joined values
#' get_complete_total(test_ct, raw = TRUE)
#' get_med_q1_q3(test_ct, raw = TRUE)
#'
NULL

# GET COMPLETE ####
#' @describeIn get_values Get the number of non-NA rows in the data
#' @export
get_complete <- function(ct_data, out_col_name = COMP_COL_NAME) {
    UseMethod("get_complete", ct_data)
}

#' @noRd
#' @export
get_complete.crosstab_data <- function(ct_data, out_col_name = COMP_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::filter(!is.na(.data[[var_name(ct_data)]])) |>
        dplyr::group_by(.data[[cohort_name(ct_data)]], .drop = FALSE) |>
        dplyr::count(name = out_col_name) |>
        data.frame(check.names = FALSE)
}

#' @noRd
#' @export
get_complete.crosstab <- function(ct_data, out_col_name = COMP_COL_NAME) {
    get_complete(data_table(ct_data), out_col_name = out_col_name)
}

# GET TOTAL ####
#' @describeIn get_values Get the number of rows in the data (including NA)
#' @export
get_total <- function(ct_data, out_col_name = TOTAL_COL_NAME) {
    UseMethod("get_total", ct_data)
}

#' @noRd
#' @export
get_total.crosstab_data <- function(ct_data, out_col_name = TOTAL_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]], .drop = FALSE) |>
        dplyr::count(name = out_col_name) |>
        data.frame(check.names = FALSE)
}

#' @noRd
#' @export
get_total.crosstab <- function(ct_data, out_col_name = TOTAL_COL_NAME) {
    get_total(data_table(ct_data), out_col_name = out_col_name)
}

# GET COMPLETE TOTAL ####
#' @describeIn get_values Get a string formatted as "[complete] / [total]" for each cohort
#' @export
get_complete_total <- function(ct_data, out_col_name = COMP_TOT_COL_NAME, raw = FALSE) {
    UseMethod("get_complete_total", ct_data)
}

#' @noRd
#' @export
get_complete_total.crosstab <- function(ct_data, out_col_name = COMP_TOT_COL_NAME, raw = FALSE) {
    get_complete_total(data_table(ct_data), out_col_name = out_col_name, raw = raw)
}

#' @noRd
#' @export
get_complete_total.crosstab_data <- function(ct_data, out_col_name = COMP_TOT_COL_NAME, raw = FALSE) {
    validate_out_col_name(out_col_name, ct_data)

    comp_col <- get_non_matching(COMP_COL_NAME, c(cohort_name(ct_data)))
    tot_col <- get_non_matching(TOTAL_COL_NAME, c(cohort_name(ct_data)))
    comp_tot <- join_val(
        get_complete(ct_data, out_col_name = comp_col),
        get_total(ct_data, out_col_name = tot_col)
    )

    if (raw) return(comp_tot)

    comp_tot[[out_col_name]] <- sprintf(
        "%s / %s",
        comp_tot[[comp_col]],
        comp_tot[[tot_col]]
    )

    keep_columns <- c(cohort_name(ct_data), out_col_name)
    comp_tot <- comp_tot[, keep_columns, drop = FALSE]

    return(comp_tot)
}

# GET MEAN ####
#' @describeIn get_values Get the means for each cohort
#' @export
get_mean <- function(ct_data, out_col_name = MEAN_COL_NAME, round_to = MEAN_ROUND_TO) {
    UseMethod("get_mean")
}

#' @noRd
#' @export
get_mean.crosstab <- function(ct_data, out_col_name = MEAN_COL_NAME, round_to = MEAN_ROUND_TO) {
    get_mean(data_table(ct_data), round_to = round_to, out_col_name = out_col_name)
}

#' @noRd
#' @export
get_mean.crosstab_data_num <- function(ct_data, out_col_name = MEAN_COL_NAME, round_to = MEAN_ROUND_TO) {
    validate_out_col_name(out_col_name, ct_data)
    validate_round_to(round_to)
    result <- ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := base::mean(
                .data[[var_name(ct_data)]],
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = FALSE)

    # Round if applicable
    if (!is.null(round_to))
        result[[out_col_name]] <- round(result[[out_col_name]], digits = round_to)

    return(result)
}

#' @noRd
#' @export
get_mean.crosstab_data <- function(ct_data, out_col_name = MEAN_COL_NAME, round_to = MEAN_ROUND_TO) {
    get_mean(as.crosstab.num(ct_data), round_to = round_to, out_col_name = out_col_name)
}

# GET SD ####
#' @describeIn get_values Get the standard deviations for each cohort
#' @export
get_sd <- function(ct_data, out_col_name = SD_COL_NAME, round_to = SD_ROUND_TO) {
    UseMethod("get_sd", ct_data)
}

#' @noRd
#' @export
get_sd.crosstab <- function(ct_data, out_col_name = SD_COL_NAME, round_to = SD_ROUND_TO) {
    get_sd(data_table(ct_data), round_to = round_to, out_col_name = out_col_name)
}

#' @noRd
#' @export
get_sd.crosstab_data_num <- function(ct_data, out_col_name = SD_COL_NAME, round_to = SD_ROUND_TO) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)
    result <- ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := stats::sd(
                .data[[var_name(ct_data)]],
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = FALSE)

    # Round if applicable
    if (!is.null(round_to))
        result[[out_col_name]] <- round(result[[out_col_name]], digits = round_to)

    return(result)
}

#' @noRd
#' @export
get_sd.crosstab_data <- function(ct_data, out_col_name = SD_COL_NAME, round_to = SD_ROUND_TO) {
    get_sd(as.crosstab.num(ct_data), round_to = round_to, out_col_name = out_col_name)
}

# GET MEAN SD ####
#' @describeIn get_values Get a string formatted as "[mean] +/- [sd]" for each cohort
#' @export
get_mean_sd <- function(ct_data, out_col_name = MEAN_SD_COL_NAME, round_to = MEAN_SD_ROUND_TO, raw = FALSE) {
    UseMethod("get_mean_sd", ct_data)
}

#' @noRd
#' @export
get_mean_sd.crosstab <- function(ct_data, out_col_name = MEAN_SD_COL_NAME, round_to = MEAN_SD_ROUND_TO, raw = FALSE) {
    get_mean_sd(data_table(ct_data), out_col_name = out_col_name, round_to = round_to, raw = raw)
}

#' @noRd
#' @export
get_mean_sd.crosstab_data <- function(ct_data, out_col_name = MEAN_SD_COL_NAME, round_to = MEAN_SD_ROUND_TO, raw = FALSE) {
    get_mean_sd(as.crosstab.num(ct_data), out_col_name = out_col_name, round_to = round_to, raw = raw)
}

#' @noRd
#' @export
get_mean_sd.crosstab_data_num <- function(ct_data, out_col_name = MEAN_SD_COL_NAME, round_to = MEAN_SD_ROUND_TO, raw = FALSE) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    mean_col <- get_non_matching(MEAN_COL_NAME, cohort_name(ct_data))
    sd_col <- get_non_matching(SD_COL_NAME, cohort_name(ct_data))
    mean_sd <- join_val(
        get_mean(ct_data, out_col_name = mean_col, round_to = round_to),
        get_sd(ct_data, out_col_name = sd_col, round_to = round_to)
    )

    if (raw) return(mean_sd)

    mean_sd[[out_col_name]] <- sprintf(
        "%s \u00b1 %s", # This is unicode for the plus-minus symbol
        mean_sd[[mean_col]],
        mean_sd[[sd_col]]
    )

    keep_columns <- c(cohort_name(ct_data), out_col_name)
    mean_sd <- mean_sd[, keep_columns, drop = FALSE]

    return(mean_sd)
}

# GET MEDIAN ####
#' @describeIn get_values Get the median for each cohort (same as `get_med()`)
#' @export
get_median <- function(ct_data, out_col_name = MED_COL_NAME, round_to = MEDIAN_ROUND_TO) {
    get_med(ct_data = ct_data, out_col_name = out_col_name, round_to = round_to)
}

#' @describeIn get_values Get the median for each cohort (same as `get_median()`)
#' @export
get_med <- function(ct_data, out_col_name = MED_COL_NAME, round_to = MEDIAN_ROUND_TO) {
    UseMethod("get_med", ct_data)
}

#' @noRd
#' @export
get_med.crosstab <- function(ct_data, out_col_name = MED_COL_NAME, round_to = MEDIAN_ROUND_TO) {
    get_med(data_table(ct_data), round_to = round_to, out_col_name = out_col_name)
}

#' @noRd
#' @export
get_med.crosstab_data_num <- function(ct_data, out_col_name = MED_COL_NAME, round_to = MEDIAN_ROUND_TO) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)
    result <- ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := stats::median(
                .data[[var_name(ct_data)]],
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = FALSE)

    # Round if applicable
    if (!is.null(round_to))
        result[[out_col_name]] <- round(result[[out_col_name]], digits = round_to)

    return(result)
}

#' @noRd
#' @export
get_med.crosstab_data <- function(ct_data, out_col_name = MED_COL_NAME, round_to = MEDIAN_ROUND_TO) {
    get_med(as.crosstab.num(ct_data), round_to = round_to, out_col_name = out_col_name)
}

# GET Q1 ####
#' @describeIn get_values Get the first quartile for each cohort
#' @export
get_q1 <- function(ct_data, out_col_name = Q1_COL_NAME, round_to = Q1_ROUND_TO) {
    UseMethod("get_q1", ct_data)
}

#' @noRd
#' @export
get_q1.crosstab <- function(ct_data, out_col_name = Q1_COL_NAME, round_to = Q1_ROUND_TO) {
    get_q1(data_table(ct_data), round_to = round_to, out_col_name = out_col_name)
}

#' @noRd
#' @export
get_q1.crosstab_data_num <- function(ct_data, out_col_name = Q1_COL_NAME, round_to = Q1_ROUND_TO) {
    validate_out_col_name(out_col_name, ct_data)
    validate_round_to(round_to)
    result <- ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := stats::quantile(
                .data[[var_name(ct_data)]],
                1/4,
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = FALSE)

    # Round if applicable
    if (!is.null(round_to))
        result[[out_col_name]] <- round(result[[out_col_name]], digits = round_to)

    return(result)
}

#' @noRd
#' @export
get_q1.crosstab_data <- function(ct_data, out_col_name = Q1_COL_NAME, round_to = Q1_ROUND_TO) {
    get_q1(as.crosstab.num(ct_data), round_to = round_to, out_col_name = out_col_name)
}

# GET Q3 ####
#' @describeIn get_values Get the third quartile for each cohort
#' @export
get_q3 <- function(ct_data, out_col_name = Q3_COL_NAME, round_to = Q3_ROUND_TO) {
    UseMethod("get_q3", ct_data)
}

#' @noRd
#' @export
get_q3.crosstab <- function(ct_data, out_col_name = Q3_COL_NAME, round_to = Q3_ROUND_TO) {
    get_q3(data_table(ct_data), round_to = round_to, out_col_name = out_col_name)
}

#' @noRd
#' @export
get_q3.crosstab_data_num <- function(ct_data, out_col_name = Q3_COL_NAME, round_to = Q3_ROUND_TO) {
    validate_out_col_name(out_col_name, ct_data)
    validate_round_to(round_to)
    result <- ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := stats::quantile(
                .data[[var_name(ct_data)]],
                3/4,
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = FALSE)

    # Round if applicable
    if (!is.null(round_to))
        result[[out_col_name]] <- round(result[[out_col_name]], digits = round_to)

    return(result)
}

#' @noRd
#' @export
get_q3.crosstab_data <- function(ct_data, out_col_name = Q3_COL_NAME, round_to = Q3_ROUND_TO) {
    get_q3(as.crosstab.num(ct_data), round_to = round_to, out_col_name = out_col_name)
}

# GET Q1 Q3 ####
#' @describeIn get_values Get a string formatted as "[Q1]--[Q3]" for each cohort
#' @export
get_q1_q3 <- function(ct_data, out_col_name = Q1_Q3_COL_NAME, round_to = Q1_Q3_ROUND_TO, raw = FALSE) {
    UseMethod("get_q1_q3", ct_data)
}

#' @noRd
#' @export
get_q1_q3.crosstab <- function(ct_data, out_col_name = Q1_Q3_COL_NAME, round_to = Q1_Q3_ROUND_TO, raw = FALSE) {
    get_q1_q3(data_table(ct_data), out_col_name = out_col_name, round_to = round_to, raw = raw)
}

#' @noRd
#' @export
get_q1_q3.crosstab_data <- function(ct_data, out_col_name = Q1_Q3_COL_NAME, round_to = Q1_Q3_ROUND_TO, raw = FALSE) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    q1_col <- get_non_matching(Q1_COL_NAME, c(cohort_name(ct_data)))
    q3_col <- get_non_matching(Q3_COL_NAME, c(cohort_name(ct_data)))
    q1_q3 <- join_val(
        get_q1(ct_data, out_col_name = q1_col, round_to = round_to),
        get_q3(ct_data, out_col_name = q3_col, round_to = round_to)
    )

    if (raw) return(q1_q3)

    q1_q3[[out_col_name]] <- sprintf(
        "%s\u2014%s",
        q1_q3[[q1_col]],
        q1_q3[[q3_col]]
    )

    keep_columns <- c(cohort_name(ct_data), out_col_name)
    q1_q3 <- q1_q3[, keep_columns, drop = FALSE]

    return(q1_q3)
}

# GET IQR ####
#' @describeIn get_values Get the IQR for each cohort
#' @export
get_iqr <- function(ct_data, out_col_name = IQR_COL_NAME, round_to = IQR_ROUND_TO) {
    UseMethod("get_iqr", ct_data)
}

#' @noRd
#' @export
get_iqr.crosstab <- function(ct_data, out_col_name = IQR_COL_NAME, round_to = IQR_ROUND_TO) {
    get_iqr(data_table(ct_data), out_col_name = out_col_name, round_to = round_to)
}

#' @noRd
#' @export
get_iqr.crosstab_data <- function(ct_data, out_col_name = IQR_COL_NAME, round_to = IQR_ROUND_TO) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    q1_col <- get_non_matching(Q1_COL_NAME, c(cohort_name(ct_data)))
    q3_col <- get_non_matching(Q3_COL_NAME, c(cohort_name(ct_data)))
    iqr <- join_val(
        get_q1(ct_data, out_col_name = q1_col, round_to = round_to),
        get_q3(ct_data, out_col_name = q3_col, round_to = round_to)
    )

    iqr[[out_col_name]] <- iqr[[q3_col]] - iqr[[q1_col]]

    keep_columns <- c(cohort_name(ct_data), out_col_name)
    iqr <- iqr[, keep_columns, drop = FALSE]

    return(iqr)
}

# GET IQR Q3-Q1 COL ####
#' @describeIn get_values Get a string formatted as "[IQR] ([Q3]-[Q1])" for each cohort
#' @export
get_iqr_q3_q1 <- function(ct_data, out_col_name = IQR_Q3_Q1_COL_NAME, round_to = IQR_Q3_Q1_ROUND_TO, raw = FALSE) {
    UseMethod("get_iqr_q3_q1", ct_data)
}

#' @noRd
#' @export
get_iqr_q3_q1.crosstab <- function(ct_data, out_col_name = IQR_Q3_Q1_COL_NAME, round_to = IQR_Q3_Q1_ROUND_TO, raw = FALSE) {
    get_iqr_q3_q1(data_table(ct_data), out_col_name = out_col_name, round_to = round_to, raw = raw)
}

#' @noRd
#' @export
get_iqr_q3_q1.crosstab_data <- function(ct_data, out_col_name = IQR_Q3_Q1_COL_NAME, round_to = IQR_Q3_Q1_ROUND_TO, raw = FALSE) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    iqr_col <- get_non_matching(IQR_COL_NAME, c(cohort_name(ct_data)))
    q1_col <- get_non_matching(Q1_COL_NAME, c(cohort_name(ct_data)))
    q3_col <- get_non_matching(Q3_COL_NAME, c(cohort_name(ct_data)))
    iqr_q3_q1 <- join_val(
        get_iqr(ct_data, out_col_name = iqr_col, round_to = round_to),
        get_q3(ct_data, out_col_name = q3_col, round_to = round_to),
        get_q1(ct_data, out_col_name = q1_col, round_to = round_to)
    )

    if (raw) return(iqr_q3_q1)

    iqr_q3_q1[[out_col_name]] <- sprintf(
        "%s (%s-%s)",
        iqr_q3_q1[[iqr_col]],
        iqr_q3_q1[[q3_col]],
        iqr_q3_q1[[q1_col]]
    )

    keep_columns <- c(cohort_name(ct_data), out_col_name)
    iqr_q3_q1 <- iqr_q3_q1[, keep_columns, drop = FALSE]

    return(iqr_q3_q1)
}

# GET MED Q1 Q3 ####
#' @describeIn get_values Get a string formatted as "[median] ([Q1], [Q3])" for each cohort
#' @export
get_med_q1_q3 <- function(ct_data, out_col_name = MED_Q1_Q3_COL_NAME, round_to = MED_Q1_Q3_ROUND_TO, raw = FALSE) {
    UseMethod("get_med_q1_q3", ct_data)
}

#' @noRd
#' @export
get_med_q1_q3.crosstab <- function(ct_data, out_col_name = MED_Q1_Q3_COL_NAME, round_to = MED_Q1_Q3_ROUND_TO, raw = FALSE) {
    get_med_q1_q3(data_table(ct_data), out_col_name = out_col_name, round_to = round_to, raw = raw)
}

#' @noRd
#' @export
get_med_q1_q3.crosstab_data <- function(ct_data, out_col_name = MED_Q1_Q3_COL_NAME, round_to = MED_Q1_Q3_ROUND_TO, raw = FALSE) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    med_col <- get_non_matching(MED_COL_NAME, c(cohort_name(ct_data)))
    q1_col <- get_non_matching(Q1_COL_NAME, c(cohort_name(ct_data)))
    q3_col <- get_non_matching(Q3_COL_NAME, c(cohort_name(ct_data)))
    med_q1_q3 <- join_val(
        get_med(ct_data, out_col_name = med_col, round_to = round_to),
        get_q1(ct_data, out_col_name = q1_col, round_to = round_to),
        get_q3(ct_data, out_col_name = q3_col, round_to = round_to)
    )

    if (raw) return(med_q1_q3)

    med_q1_q3[[out_col_name]] <- sprintf(
        "%s (%s, %s)",
        med_q1_q3[[med_col]],
        med_q1_q3[[q1_col]],
        med_q1_q3[[q3_col]]
    )

    keep_columns <- c(cohort_name(ct_data), out_col_name)
    med_q1_q3 <- med_q1_q3[, keep_columns, drop = FALSE]

    return(med_q1_q3)
}

# GET COUNT ####
#' @describeIn get_values Get the counts for each variable and cohort
#' @export
get_count <- function(ct_data, out_col_name = COUNT_COL_NAME, keep_na_vars = FALSE) {
    UseMethod("get_count", ct_data)
}

#' @noRd
#' @export
get_count.crosstab <- function(ct_data, out_col_name = COUNT_COL_NAME, keep_na_vars = FALSE) {
    get_count(data_table(ct_data), out_col_name = out_col_name, keep_na_vars = keep_na_vars)
}

#' @noRd
#' @export
get_count.crosstab_data <- function(ct_data, out_col_name = COUNT_COL_NAME, keep_na_vars = FALSE) {
    get_count(suppressWarnings(as.crosstab.cat(ct_data)), out_col_name = out_col_name, keep_na_vars = keep_na_vars)
}

#' @noRd
#' @export
get_count.crosstab_data_cat <- function(ct_data, out_col_name = COUNT_COL_NAME, keep_na_vars = FALSE) {
    validate_input_get_count(ct_data, out_col_name, keep_na_vars)

    if (!keep_na_vars) {
        keep <- !is.na(var(ct_data))
        ct_data <- ct_data[keep, , drop = FALSE]
    }

    result <- ct_data |>
        dplyr::group_by(
            .data[[cohort_name(ct_data)]],
            .data[[var_name(ct_data)]],
            .drop = FALSE
        ) |>
        dplyr::count(
            name = out_col_name
        ) |>
        data.frame(check.names = FALSE)

    return(result)
}

# GET PROPORTION ####
#' @describeIn get_values Get the proportions (out of complete) for each variable and cohort (same as `get_prop()`)
#' @export
get_proportion <- function(ct_data, out_col_name = PROP_COL_NAME, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    get_prop(ct_data = ct_data, out_col_name = out_col_name, round_to = round_to, keep_na_vars = keep_na_vars)
}

#' @describeIn get_values Get the proportions (out of complete) for each variable and cohort (same as `get_proportion()`)
#' @export
get_prop <- function(ct_data, out_col_name = PROP_COL_NAME, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    UseMethod("get_prop", ct_data)
}

#' @noRd
#' @export
get_prop.crosstab <- function(ct_data, out_col_name = PROP_COL_NAME, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    get_prop(data_table(ct_data), round_to = round_to, out_col_name = out_col_name, keep_na_vars = keep_na_vars)
}

#' @noRd
#' @export
get_prop.crosstab_data_cat <- function(ct_data, out_col_name = PROP_COL_NAME, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    # Don't check for types a) because there's no need, it's a polymorphic
    # function, and b) because I need to pass other types in like multi.
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    count_col <- get_non_matching(COUNT_COL_NAME, c(var_name(ct_data), cohort_name(ct_data)))
    complete_col <- get_non_matching(COMP_COL_NAME, c(var_name(ct_data), cohort_name(ct_data), count_col))
    props <- join_val(
        get_count(ct_data, out_col_name = count_col, keep_na_vars = keep_na_vars),
        get_complete(ct_data, out_col_name = complete_col)
    )

    props[[out_col_name]] <- ifelse(props[[complete_col]] == 0, 0, props[[count_col]] / props[[complete_col]])

    # Round if applicable
    if (!is.null(round_to))
        props[[out_col_name]] <- round(props[[out_col_name]], digits = round_to)

    props <- props[, c(cohort_name(ct_data), var_name(ct_data), out_col_name), drop = FALSE]

    # NA variables shouldn't have a prop, because props are calculated
    # out of those who completed the question, and if they are NA then they
    # aren't part of that count.

    na_answers <- is.na(props[[var_name(ct_data)]])
    props[[out_col_name]][na_answers] <- NA

    return(props)
}

#' @noRd
#' @export
get_prop.crosstab_data_multi <- function(ct_data, out_col_name = PROP_COL_NAME, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    get_prop.crosstab_data_cat(ct_data, round_to = round_to, out_col_name = out_col_name, keep_na_vars = keep_na_vars)
}

#' @noRd
#' @export
get_prop.crosstab_data <- function(ct_data, out_col_name = PROP_COL_NAME, round_to = PROP_ROUND_TO, keep_na_vars = FALSE) {
    get_prop(suppressWarnings(as.crosstab.cat(ct_data)), round_to = round_to, out_col_name = out_col_name, keep_na_vars = keep_na_vars)
}

# GET COUNT PROPORTION ####
#' @describeIn get_values Get a string formatted as "[count] ([prop])" for each response and cohort
#' @export
get_count_prop <- function(ct_data, out_col_name = COUNT_PROP_COL_NAME, round_to = COUNT_PROP_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    UseMethod("get_count_prop", ct_data)
}

#' @noRd
#' @export
get_count_prop.crosstab <- function(ct_data, out_col_name = COUNT_PROP_COL_NAME, round_to = COUNT_PROP_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    get_count_prop(data_table(ct_data), out_col_name = out_col_name, round_to = round_to, keep_na_vars = keep_na_vars, raw = raw)
}

#' @noRd
#' @export
get_count_prop.crosstab_data <- function(ct_data, out_col_name = COUNT_PROP_COL_NAME, round_to = COUNT_PROP_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    count_col <- get_non_matching(COUNT_COL_NAME, c(cohort_name(ct_data), var_name(ct_data)))
    prop_col <- get_non_matching(PROP_COL_NAME, c(cohort_name(ct_data), var_name(ct_data)))
    count_prop <- join_val(
        get_count(ct_data, out_col_name = count_col, keep_na_vars = keep_na_vars),
        get_prop(ct_data, out_col_name = prop_col, round_to = round_to, keep_na_vars = keep_na_vars)
    )

    if (raw) return(count_prop)

    make_count_prop <- function(count, prop) {
        na_vals <- is.na(count) | is.na(prop)
        combined <- sprintf("%s (%s)", count, prop)
        combined[na_vals] <- count[na_vals]
        combined
    }

    count_prop[[out_col_name]] <- make_count_prop(
        count_prop[[count_col]],
        count_prop[[prop_col]]
    )

    keep_columns <- c(cohort_name(ct_data), var_name(ct_data), out_col_name)
    count_prop <- count_prop[, keep_columns, drop = FALSE]

    return(count_prop)
}

# GET PERCENT ####
#' @describeIn get_values Get the percents (out of complete) for each variable and cohort
#' @export
get_percent <- function(ct_data, out_col_name = PERCENT_COL_NAME, round_to = PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    UseMethod("get_percent", ct_data)
}

#' @noRd
#' @export
get_percent.crosstab <- function(ct_data, out_col_name = PERCENT_COL_NAME, round_to = PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    get_percent(data_table(ct_data), round_to = round_to, out_col_name = out_col_name, keep_na_vars = keep_na_vars, raw = raw)
}

#' @noRd
#' @export
get_percent.crosstab_data <- function(ct_data, out_col_name = PERCENT_COL_NAME, round_to = PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    # rounding 24% to 0 decimal places requires rounding 0.24 to 2 decimal places
    if (!is.null(round_to))
        round_to <- round_to + 2

    percents <- get_proportion(ct_data = ct_data, out_col_name = out_col_name, round_to = round_to, keep_na_vars = keep_na_vars)
    percents[[out_col_name]] <- 100 * percents[[out_col_name]]

    make_percent_str <- function(percent) {
        na_vals <- is.na(percent)
        percent <- paste0(percent, "%")
        percent[na_vals] <- NA
        percent
    }

    if (!raw) {
        percents[[out_col_name]] <- make_percent_str(percents[[out_col_name]])
    }

    return(percents)
}

# GET COUNT PERCENT ####
#' @describeIn get_values Get a string formatted as "[count] ([percent]\%)" for each response and cohort
#' @export
get_count_percent <- function(ct_data, out_col_name = COUNT_PERCENT_COL_NAME, round_to = COUNT_PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    UseMethod("get_count_percent", ct_data)
}

#' @noRd
#' @export
get_count_percent.crosstab <- function(ct_data, out_col_name = COUNT_PERCENT_COL_NAME, round_to = COUNT_PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    get_count_percent(data_table(ct_data), out_col_name = out_col_name, round_to = round_to, keep_na_vars = keep_na_vars, raw = raw)
}

#' @noRd
#' @export
get_count_percent.crosstab_data <- function(ct_data, out_col_name = COUNT_PERCENT_COL_NAME, round_to = COUNT_PERCENT_ROUND_TO, keep_na_vars = FALSE, raw = FALSE) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)

    count_col <- get_non_matching(COUNT_COL_NAME, c(cohort_name(ct_data), var_name(ct_data)))
    percent_col <- get_non_matching(PERCENT_COL_NAME, c(cohort_name(ct_data), var_name(ct_data)))
    count_percent <- join_val(
        get_count(ct_data, out_col_name = count_col, keep_na_vars = keep_na_vars),
        get_percent(ct_data, out_col_name = percent_col, round_to = round_to, keep_na_vars = keep_na_vars, raw = raw)
    )

    if (raw) return(count_percent)

    make_count_percent <- function(count, percent) {
        na_vals <- is.na(count) | is.na(percent)
        combined <- sprintf("%s (%s)", count, percent)
        combined[na_vals] <- count[na_vals]
        combined
    }

    count_percent[[out_col_name]] <- make_count_percent(
        count_percent[[count_col]],
        count_percent[[percent_col]]
    )

    keep_columns <- c(cohort_name(ct_data), var_name(ct_data), out_col_name)
    count_percent <- count_percent[, keep_columns, drop = FALSE]

    return(count_percent)
}

