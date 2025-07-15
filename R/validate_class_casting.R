# IMPORTS ####
#' @import assertthat

validate_input_as_likert <- function(ct_data, var_map) {
    UseMethod("validate_input_as_likert", ct_data)
}

#' @export
validate_input_as_likert.crosstab_data_cat <- function(ct_data, var_map) {
    assert_that(
        !is.null(var_map),
        msg = "var_map is required to convert to likert data"
    )
    assert_that(
        is.numeric(var_map),
        !is.null(names(var_map)),
        msg = "var_map must be a named numeric vector"
    )
    assert_that(
        all(var(ct_data) %in% c(names(var_map), NA)),
        msg = "Detected values in variable column that aren't in var_map"
    )
}

#' @export
validate_input_as_likert.crosstab_data_num <- function(ct_data, var_map) {
    assert_that(
        !is.null(var_map),
        msg = "var_map is required to convert to likert data"
    )
    assert_that(
        is.numeric(var_map),
        !is.null(names(var_map)),
        msg = "var_map must be a named numeric vector"
    )
    assert_that(
        all(var(ct_data) %in% c(var_map, NA)),
        msg = "Detected values in variable column that aren't in var_map"
    )
}

#' @export
validate_input_as_likert.crosstab_data_likert <- function(ct_data, var_map) {
    if (!is.null(var_map)) {
        assert_that(
            is.numeric(var_map),
            !is.null(names(var_map)),
            msg = "var_map must be a named numeric vector"
        )
        assert_that(
            all(var(ct_data) %in% c(names(var_map), NA)),
            msg = "Detected values in variable column that aren't in var_map"
        )
    }
}

#' @export
validate_input_as_likert.crosstab_data_multi <- function(ct_data, var_map) {
    assert_that(
        !is.null(var_map),
        msg = "var_map is required to convert to likert data"
    )
    assert_that(
        is.numeric(var_map),
        !is.null(names(var_map)),
        msg = "var_map must be a named numeric vector"
    )
    assert_that(
        all(unlist(var(ct_data)) %in% c(names(var_map), NA)),
        msg = "Detected values in variable column that aren't in var_map"
    )
}
