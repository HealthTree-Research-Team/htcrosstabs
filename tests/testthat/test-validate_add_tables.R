test_that("validate_input_add_default_table() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_default_table(ct, 1L, 2L, 3L))
})

test_that("validate_input_add_default_table() errors if ct is not a crosstab", {
    expect_error(validate_input_add_default_table("not_ct", 1L, 2L, 3L))
})

test_that("validate_input_add_default_table() errors if round_mean_sd_to is not integer", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_error(validate_input_add_default_table(ct, "1", 2L, 3L))
})

test_that("validate_input_add_default_table() errors if round_med_iqr_to is not integer", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_error(validate_input_add_default_table(ct, 1L, "2", 3L))
})

test_that("validate_input_add_default_table() errors if round_percent_to is not integer", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_error(validate_input_add_default_table(ct, 1L, 2L, "3"))
})
