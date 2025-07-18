test_that("validate_input_add_default_table() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_default_table(ct, F, 1L, 2L, 3L))
})

test_that("validate_input_add_default_table() errors if ct is not a crosstab", {
    expect_error(validate_input_add_default_table("not_ct", F, 1L, 2L, 3L))
})

test_that("validate_input_add_default_table() errors if round_mean_sd_to is not integer", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_error(validate_input_add_default_table(ct, "1", F, 2L, 3L))
})

test_that("validate_input_add_default_table() errors if round_med_iqr_to is not integer", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_error(validate_input_add_default_table(ct, F, 1L, "2", 3L))
})

test_that("validate_input_add_default_table() errors if round_percent_to is not integer", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_error(validate_input_add_default_table(ct, F, 1L, 2L, "3"))
})

test_that("validate_input_default_stacked_crosstab() works when given proper data",{
    test_df <- cat_test_df(col_name = "cat")
    test_df[["lik"]] <- lik_test_df()[["variable"]]
    test_map <- default_var_map(test_df[["lik"]])

    test_map_list <- list(
        cat = default_var_map(test_df[["cat"]]),
        lik = default_var_map(test_df[["lik"]])
    )

    expect_silent(validate_input_default_stacked_crosstab(test_df, NULL, var_map = NULL))
    expect_silent(validate_input_default_stacked_crosstab(test_df, "cohort", var_map = NULL))
    expect_silent(validate_input_default_stacked_crosstab(test_df, NULL, var_map = test_map))
    expect_silent(validate_input_default_stacked_crosstab(test_df, "cohort", var_map = test_map))
    expect_silent(validate_input_default_stacked_crosstab(test_df, NULL, var_map = test_map_list))
    expect_silent(validate_input_default_stacked_crosstab(test_df, "cohort", var_map = test_map_list))
})

test_that("validate_input_default_stacked_crosstab() fails when df is not a data frame or has no variable columns",{
    expect_error(validate_input_default_stacked_crosstab(NULL, NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab(TRUE, NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab(1, NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab(c(1, 2, 3), NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab("a", NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab(c("a", "b", "c"), NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab(list(), NULL, NULL))

    # Data frame has no variable columns
    expect_error(validate_input_default_stacked_crosstab(data.frame(), NULL, NULL))
    expect_error(validate_input_default_stacked_crosstab(data.frame(a = 1), "a", NULL))
})

test_that("validate_input_default_stacked_crosstab() fails when cohort_col_name is not a character",{
    test_df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))

    expect_error(validate_input_default_stacked_crosstab(test_df, TRUE, var_map = NULL))
    expect_error(validate_input_default_stacked_crosstab(test_df, 1, var_map = NULL))
    expect_error(validate_input_default_stacked_crosstab(test_df, c(1, 2, 3), var_map = NULL))
    expect_error(validate_input_default_stacked_crosstab(test_df, list(), var_map = NULL))
    expect_error(validate_input_default_stacked_crosstab(test_df, data.frame(), var_map = NULL))
})

test_that("validate_input_default_stacked_crosstab() fails when cohort_col_name is not a column name in df",{
    test_df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
    expect_error(validate_input_default_stacked_crosstab(test_df, "d", var_map = NULL))
})

test_that("validate_input_default_stacked_crosstab() fails when var_map is not a named numeric vector",{
    test_df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
    expect_error(validate_input_default_stacked_crosstab(test_df, NULL, var_map = c(1, 2, 3)))
    expect_error(validate_input_default_stacked_crosstab(test_df, NULL, var_map = c("a", "b", "c")))
})

