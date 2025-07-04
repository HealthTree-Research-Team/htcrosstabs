# IMPORTS ####
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom rlang .data

# GET TOTAL ####
#' @export
get_total <- function(ct_data, out_col_name = TOTAL_COL_NAME) {
    UseMethod("get_total", ct_data)
}

#' @export
get_total.crosstab_data <- function(ct_data, out_col_name = TOTAL_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]], .drop = FALSE) |>
        dplyr::count(name = out_col_name) |>
        data.frame(check.names = F)
}

#' @export
get_total.crosstab <- function(ct_data, out_col_name = TOTAL_COL_NAME) {
    get_total(get_data(ct_data), out_col_name = out_col_name)
}

# GET COMPLETE ####
#' @export
get_complete <- function(ct_data, out_col_name = COMP_COL_NAME) {
    UseMethod("get_complete", ct_data)
}

#' @export
get_complete.crosstab_data <- function(ct_data, out_col_name = COMP_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::filter(!is.na(.data[[var_name(ct_data)]])) |>
        dplyr::group_by(.data[[cohort_name(ct_data)]], .drop = FALSE) |>
        dplyr::count(name = out_col_name) |>
        data.frame(check.names = F)
}

#' @export
get_complete.crosstab <- function(ct_data, out_col_name = COMP_COL_NAME) {
    get_complete(get_data(ct_data), out_col_name = out_col_name)
}

# GET MEAN ####
#' @export
get_mean <- function(ct_data, round_to = ROUND_MEAN_TO, out_col_name = MEAN_COL_NAME) {
    UseMethod("get_mean")
}

#' @export
get_mean.crosstab <- function(ct_data, round_to = ROUND_MEAN_TO, out_col_name = MEAN_COL_NAME) {
    get_mean(get_data(ct_data), round_to = round_to, out_col_name = out_col_name)
}

#' @export
get_mean.crosstab_data_num <- function(ct_data, round_to = ROUND_MEAN_TO, out_col_name = MEAN_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := round(
                base::mean(
                    .data[[var_name(ct_data)]],
                    na.rm = TRUE
                ),
                digits = round_to
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = F)
}

#' @export
get_mean.crosstab_data <- function(ct_data, round_to = ROUND_MEAN_TO, out_col_name = MEAN_COL_NAME) {
    get_mean(as.crosstab.num(ct_data), out_col_name = out_col_name)
}

# GET SD ####
#' @export
get_sd <- function(ct_data, round_to = ROUND_SD_TO, out_col_name = SD_COL_NAME) {
    UseMethod("get_sd", ct_data)
}

#' @export
get_sd.crosstab <- function(ct_data, round_to = ROUND_SD_TO, out_col_name = SD_COL_NAME) {
    get_sd(get_data(ct_data), out_col_name = out_col_name)
}

#' @export
get_sd.crosstab_data_num <- function(ct_data, round_to = ROUND_SD_TO, out_col_name = SD_COL_NAME) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := round(
                stats::sd(
                    .data[[var_name(ct_data)]],
                    na.rm = TRUE
                ),
                digits = round_to
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = F)
}

#' @export
get_sd.crosstab_data <- function(ct_data, round_to = ROUND_SD_TO, out_col_name = SD_COL_NAME) {
    get_sd(as.crosstab.num(ct_data), out_col_name = out_col_name)
}

# GET MEDIAN ####
#' @export
get_med <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = MED_COL_NAME) {
    UseMethod("get_med", ct_data)
}

#' @export
get_med.crosstab <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = MED_COL_NAME) {
    get_med(get_data(ct_data), out_col_name = out_col_name)
}

#' @export
get_med.crosstab_data_num <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = MED_COL_NAME) {
    validate_round_to(round_to)
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := round(
                stats::median(
                    .data[[var_name(ct_data)]],
                    na.rm = TRUE
                ),
                digits = round_to
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = F)
}

#' @export
get_med.crosstab_data <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = MED_COL_NAME) {
    get_med(as.crosstab.num(ct_data), out_col_name = out_col_name)
}

# GET Q1 ####
#' @export
get_q1 <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q1_COL_NAME) {
    UseMethod("get_q1", ct_data)
}

#' @export
get_q1.crosstab <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q1_COL_NAME) {
    get_q1(get_data(ct_data), out_col_name = out_col_name)
}

#' @export
get_q1.crosstab_data_num <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q1_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := round(
                stats::quantile(
                    .data[[var_name(ct_data)]],
                    1/4,
                    na.rm = TRUE
                ),
                digits = round_to
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = F)
}

#' @export
get_q1.crosstab_data <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q1_COL_NAME) {
    get_q1(as.crosstab.num(ct_data), out_col_name = out_col_name)
}

# GET Q3 ####
#' @export
get_q3 <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q3_COL_NAME) {
    UseMethod("get_q3", ct_data)
}

#' @export
get_q3.crosstab <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q3_COL_NAME) {
    get_q3(get_data(ct_data), out_col_name = out_col_name)
}

#' @export
get_q3.crosstab_data_num <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q3_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            !!rlang::sym(out_col_name) := round(
                stats::quantile(
                    .data[[var_name(ct_data)]],
                    3/4,
                    na.rm = TRUE
                ),
                digits = round_to
            ),
            .groups = "drop"
        ) |>
        data.frame(check.names = F)
}

#' @export
get_q3.crosstab_data <- function(ct_data, round_to = ROUND_Q1_TO, out_col_name = Q3_COL_NAME) {
    get_q3(as.crosstab.num(ct_data), out_col_name = out_col_name)
}

# GET COUNTS ####
#' @export
get_counts <- function(ct_data, out_col_name = COUNT_COL_NAME) {
    UseMethod("get_counts", ct_data)
}

#' @export
get_counts.crosstab <- function(ct_data, out_col_name = COUNT_COL_NAME) {
    get_counts(get_data(ct_data), out_col_name = out_col_name)
}

#' @export
get_counts.crosstab_data_cat <- function(ct_data, out_col_name = COUNT_COL_NAME) {
    validate_out_col_name(out_col_name, ct_data)
    ct_data |>
        dplyr::group_by(
            .data[[cohort_name(ct_data)]],
            .data[[var_name(ct_data)]],
            .drop = FALSE
        ) |>
        dplyr::count(
            name = out_col_name
        ) |>
        data.frame(check.names = F)
}

#' @export
get_counts.crosstab_data <- function(ct_data, out_col_name = COUNT_COL_NAME) {
    get_counts(as.crosstab.cat(ct_data), out_col_name = out_col_name)
}
