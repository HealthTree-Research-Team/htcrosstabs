
validate_add_default_table <- function(ct, round_mean_sd_to, round_med_iqr_to, round_percent_to) {
    assert_crosstab(ct)
    assert_that(is.integer(round_mean_sd_to))
    assert_that(is.integer(round_med_iqr_to))
    assert_that(is.integer(round_percent_to))
}

