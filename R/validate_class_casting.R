# IMPORTS ####
#' @import assertthat

validate_as_crosstab_likert <- function(ct_data, likert_map) {
    assert_that(
        !is.null(likert_map),
        msg = "likert_map is required to convert numeric to likert data"
    )
    assert_that(
        all(var(ct_data) %in% c(likert_map, NA)),
        msg = "Detected values in variable column that aren't in likert_map"
    )
    assert_that(
        !any(duplicated(likert_map)),
        msg = "When converting to likert, you can not have multiple names mapped to the same value"
    )
}
