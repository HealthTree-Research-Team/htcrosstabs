#' @import assertthat

total <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    nrow(ct_data)
}

complete <- function(ct_data, all = T) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    ct_data |>
        dplyr::group_by(!!sym(cohort_name(ct_data))) |>
        dplyr::summarise(complete = dplyr::n(), .groups = "drop")
}

# add_total_row <- function(ct) {
#     assert_that(inherits(ct, CT_CLASS))
#
# }
