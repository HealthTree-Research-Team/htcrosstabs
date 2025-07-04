#' @import assertthat

validate_round_to <- function(round_to) {
    assert_that(
        is.numeric(round_to),
        length(round_to) == 1,
        msg = "round_to must be numeric and must be a single value"
    )
}

validate_out_col_name <- function(out_col_name, ct_data) {
    assert_that(is.character(out_col_name))
    assert_that(
        !(out_col_name %in% names(ct_data)),
        msg = sprintf(
            "%s already in use as a column name, please select a different option with out_col_name = [name]",
            out_col_name
        )
    )
}
