# IMPORTS ####
#' @import assertthat

# CONSTANTS ####
ROUND_MEAN_TO = 1
ROUND_SD_TO = 1
ROUND_MEDIAN_TO = 1
ROUND_Q1_TO = 1
ROUND_Q3_TO = 1

# UTILITIES ####
validate_round_to <- function(round_to) {
    assert_that(
        is.numeric(round_to),
        length(round_to) == 1,
        msg = "round_to must be numeric and must be a single value"
    )
}

# GET TOTAL ####
#' @export
get_total <- function(ct) {
    UseMethod("get_total", ct)
}

#' @export
get_total.crosstab_data <- function(ct_data) {
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(total = dplyr::n(), .groups = "drop") |>
        data.frame(check.names = F)
}

#' @export
get_total.crosstab <- function(ct) {
    assert_that(has_attr(ct, "data"))
    get_total.crosstab_data(data(ct))
}

# GET COMPLETE ####
#' @export
get_complete <- function(ct) {
    UseMethod("get_complete", ct)
}

#' @export
get_complete.crosstab_data <- function(ct_data) {
    ct_data |>
        dplyr::filter(!is.na(.data[[var_name(ct_data)]])) |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(complete = dplyr::n(), .groups = "drop") |>
        data.frame(check.names = F)
}

#' @export
get_complete.crosstab <- function(ct) {
    assert_that(has_attr(ct, "data"))
    get_complete.crosstab_data(data(ct))
}

# GET MEAN ####
#' @export
get_mean <- function(ct, round_to = ROUND_MEAN_TO) {
    UseMethod("get_mean")
}

#' @export
get_mean.crosstab <- function(ct, round_to = ROUND_MEAN_TO) {
    assert_that(has_attr(ct, "data"))
    get_mean(data(ct), round_to = round_to)
}

#' @export
get_mean.crosstab_data_likert <- function(ct_data, round_to = ROUND_MEAN_TO) {
    get_mean(data_mapped(ct_data))
}

#' @export
get_mean.crosstab_data_num <- function(ct_data, round_to = ROUND_MEAN_TO) {
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            mean = round(
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

# GET SD ####
#' @export
get_sd <- function(ct, round_to = ROUND_SD_TO) {
    UseMethod("get_sd", ct)
}

#' @export
get_sd.crosstab <- function(ct, round_to = ROUND_SD_TO) {
    assert_that(has_attr(ct, "data"))
    get_sd(data(ct))
}

#' @export
get_sd.crosstab_data_likert <- function(ct_data, round_to = ROUND_SD_TO) {
    get_sd(data_mapped(ct_data))
}

#' @export
get_sd.crosstab_data_num <- function(ct_data, round_to = ROUND_SD_TO) {
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            sd = round(
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

# GET Q1 ####
#' @export
get_q1 <- function(ct, round_to = ROUND_Q1_TO) {
    UseMethod("get_q1", ct)
}

#' @export
get_q1.crosstab <- function(ct, round_to = ROUND_Q1_TO) {
    assert_that(has_attr(ct, "data"))
    get_q1(data(ct))
}

#' @export
get_q1.crosstab_data_likert <- function(ct_data, round_to = ROUND_Q1_TO) {
    get_q1(data_mapped(ct_data))
}

#' @export
get_q1.crosstab_data_num <- function(ct_data, round_to = ROUND_Q1_TO) {
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            sd = round(
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

# GET MEDIAN ####
#' @export
get_med <- function(ct, round_to = ROUND_Q1_TO) {
    UseMethod("get_med", ct)
}

#' @export
get_med.crosstab <- function(ct, round_to = ROUND_Q1_TO) {
    assert_that(has_attr(ct, "data"))
    get_med(data(ct))
}

#' @export
get_med.crosstab_data_likert <- function(ct_data, round_to = ROUND_Q1_TO) {
    get_med(data_mapped(ct_data))
}

#' @export
get_med.crosstab_data_num <- function(ct_data, round_to = ROUND_Q1_TO) {
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            sd = round(
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

# GET Q3 ####
#' @export
get_q3 <- function(ct, round_to = ROUND_Q1_TO) {
    UseMethod("get_q3", ct)
}

#' @export
get_q3.crosstab <- function(ct, round_to = ROUND_Q1_TO) {
    assert_that(has_attr(ct, "data"))
    get_q3(data(ct))
}

#' @export
get_q3.crosstab_data_likert <- function(ct_data, round_to = ROUND_Q1_TO) {
    get_q3(data_mapped(ct_data))
}

#' @export
get_q3.crosstab_data_num <- function(ct_data, round_to = ROUND_Q1_TO) {
    validate_round_to(round_to)
    ct_data |>
        dplyr::group_by(.data[[cohort_name(ct_data)]]) |>
        dplyr::summarise(
            sd = round(
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
