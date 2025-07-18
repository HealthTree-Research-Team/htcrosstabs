test_that("validate_round_to() works with valid input", {
    expect_silent(validate_round_to(1))
    expect_silent(validate_round_to(0))
    expect_silent(validate_round_to(2.5))
    expect_silent(validate_round_to(NULL))
})

test_that("validate_round_to() fails with non-numeric input", {
    expect_error(validate_round_to("a"), "round_to must be numeric")
    expect_error(validate_round_to(TRUE), "round_to must be numeric")
    expect_error(validate_round_to(list(1)), "round_to must be numeric")
})

test_that("validate_round_to() fails with vector length > 1", {
    expect_error(validate_round_to(c(1, 2)), "round_to must be numeric")
    expect_error(validate_round_to(numeric(0)), "round_to must be numeric")
})

test_that("validate_out_col_name() works with valid input", {
    df <- data.frame(a = 1:3, b = 4:6)
    expect_silent(validate_out_col_name("new_col", df))
})

test_that("validate_out_col_name() fails when out_col_name is not a character", {
    df <- data.frame(a = 1:3)
    expect_error(validate_out_col_name(1, df))
    expect_error(validate_out_col_name(TRUE, df))
    expect_error(validate_out_col_name(list("x"), df))
})

test_that("validate_out_col_name() fails if name is already in use", {
    df <- data.frame(existing = 1:3)
    expect_error(validate_out_col_name("existing", df), "already in use as a column name")
})

test_that("validate_input_get_count() works when given proper data",{
    expect_silent(validate_input_get_count(
        ct_data = crosstab_data(cat_test_df(), "cohort"),
        out_col_name = "test_col",
        keep_na_vars = F
    ))
})

test_that("validate_input_get_count() fails when keep_na_vars is not logical",{
    ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_error(validate_input_get_count(ct_data, "a", NULL))
    expect_error(validate_input_get_count(ct_data, "a", 1))
    expect_error(validate_input_get_count(ct_data, "a", c(1, 2, 3)))
    expect_error(validate_input_get_count(ct_data, "a", "a"))
    expect_error(validate_input_get_count(ct_data, "a", c("a", "b", "c")))
    expect_error(validate_input_get_count(ct_data, "a", list()))
    expect_error(validate_input_get_count(ct_data, "a", data.frame()))
    expect_error(validate_input_get_count(ct_data, "a", ct_data))
})
