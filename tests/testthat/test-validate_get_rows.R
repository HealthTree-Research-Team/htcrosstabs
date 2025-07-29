# validate_input_to_wide() ####
test_that("validate_input_to_wide() works when given proper data",{
    long_df <- factorize_columns(data.frame(
        description = "test description",
        cohort = c("a", "b", "c"),
        value = c(2, 3, 4)
    ))

    expect_silent(validate_input_to_wide(
        long_df = long_df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() works when given proper data with NA values",{
    long_df <- factorize_columns(data.frame(
        description = "test description",
        cohort = c("a", "b", "c", NA),
        value = c(2, 3, 4, 1)
    ))

    expect_silent(validate_input_to_wide(
        long_df = long_df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = 1
    ))
})

test_that("validate_input_to_wide() errors when long_df is not a data frame", {
    expect_error(validate_input_to_wide(
        long_df = "not a dataframe",
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when description_col is not a character", {
    df <- data.frame(description = "desc", cohort = "a", value = 1)
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = 123,
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when cohort_col is not a character", {
    df <- data.frame(description = "desc", cohort = "a", value = 1)
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = 123,
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when na_fill is NULL", {
    df <- data.frame(description = "desc", cohort = "a", value = 1)
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NULL
    ))
})

test_that("validate_input_to_wide() errors when long_df does not have 3 columns", {
    df <- data.frame(description = "desc", cohort = "a")
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when description_col is not in data frame", {
    df <- data.frame(not_desc = "desc", cohort = "a", value = 1)
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when cohort_col is not in data frame", {
    df <- data.frame(description = "desc", not_cohort = "a", value = 1)
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when cohort column is not a factor", {
    df <- data.frame(description = "desc", cohort = "a", value = 1)
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

test_that("validate_input_to_wide() errors when duplicate description/cohort rows exist", {
    df <- data.frame(
        description = c("desc", "desc"),
        cohort = factor(c("a", "a")),
        value = c(1, 2)
    )
    expect_error(validate_input_to_wide(
        long_df = df,
        description_col = "description",
        cohort_col = "cohort",
        na_fill = NA
    ))
})

# validate_input_to_long() ####
test_that("validate_input_to_long() works when given proper data", {
    wide_df <- data.frame(
        description = c("a", "b", "c"),
        group1 = c(1, 2, 3),
        group2 = c(4, 5, 6)
    )

    expect_silent(validate_input_to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))
})

test_that("validate_input_to_long() errors when wide_df is not a data frame", {
    expect_error(validate_input_to_long(
        wide_df = "not a df",
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))
})

test_that("validate_input_to_long() errors when description_col is not character", {
    df <- data.frame(description = "x", group = 1)
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = 123,
        cohorts_to = "cohort",
        values_to = "value"
    ))
})

test_that("validate_input_to_long() errors when cohorts_to is not character", {
    df <- data.frame(description = "x", group = 1)
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = "description",
        cohorts_to = 456,
        values_to = "value"
    ))
})

test_that("validate_input_to_long() errors when values_to is not character", {
    df <- data.frame(description = "x", group = 1)
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = 789
    ))
})

test_that("validate_input_to_long() errors when description_col is not in wide_df", {
    df <- data.frame(not_description = "x", group = 1)
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))
})

test_that("validate_input_to_long() errors when description_col, cohorts_to, values_to are not unique", {
    df <- data.frame(description = "x", group = 1)
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = "x",
        cohorts_to = "x",
        values_to = "y"
    ))
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = "x",
        cohorts_to = "y",
        values_to = "x"
    ))
    expect_error(validate_input_to_long(
        wide_df = df,
        description_col = "x",
        cohorts_to = "y",
        values_to = "y"
    ))
})

# validate_input_add_rows() ####
test_that("validate_input_add_rows() works when given proper input", {
    ct <- structure(data.frame(a = 1:3), class = c("crosstab", "data.frame"))
    rows <- data.frame(a = 4:5)

    expect_silent(validate_input_add_rows(ct, rows, NULL, "top", "bottom"))
})

test_that("validate_input_add_rows() errors when ct is not a crosstab", {
    not_ct <- data.frame(a = 1:3)
    rows <- data.frame(a = 4:5)

    expect_error(validate_input_add_rows(not_ct, rows, NULL, "top", "bottom"))
})

test_that("validate_input_add_rows() errors when rows is not a data frame", {
    ct <- structure(data.frame(a = 1:3), class = c("crosstab", "data.frame"))
    rows <- list(a = 4:5)

    expect_error(validate_input_add_rows(ct, rows, NULL, "top", "bottom"))
})

# validate_input_col_names() ####
test_that("validate_input_col_names() passes when names are all unique (long = F)", {
    test_df <- cat_test_df()
    ct <- crosstab(test_df, cohort_col_name = "cohort", desc_col_name = "description")
    expect_silent(validate_input_col_names(ct, long_out_col = "out_col", long = F))
})

test_that("validate_input_col_names() errors when desc_name is in cohort_levels (long = F)", {
    test_df <- cat_test_df()
    ct <- crosstab(test_df, cohort_col_name = "cohort", desc_col_name = character_levels[1])
    expect_error(validate_input_col_names(ct, long_out_col = "out_col", long = F))
})

test_that("validate_input_col_names() passes when names are all unique (long = T)", {
    test_df <- cat_test_df()
    ct <- crosstab(test_df, cohort_col_name = "cohort", desc_col_name = "description")
    expect_silent(validate_input_col_names(ct, long_out_col = "out_col", long = T))
})

test_that("validate_input_col_names() errors when desc_name is same as long_out_col (long = T)", {
    test_df <- cat_test_df()
    ct <- crosstab(test_df, cohort_col_name = "cohort", desc_col_name = "out_col")
    expect_error(validate_input_col_names(ct, long_out_col = "out_col", long = T))
})

# validate_input_get_complete_row() ####
test_that("validate_input_get_complete_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_complete_row(ct, TRUE, "count"))
    expect_error(validate_input_get_complete_row("not_ct", TRUE, "count"))
    expect_error(validate_input_get_complete_row(ct, "TRUE", "count"))
    expect_error(validate_input_get_complete_row(ct, TRUE, 123))
})

# validate_input_add_complete_row() ####
test_that("validate_input_add_complete_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_complete_row(ct))
    expect_error(validate_input_add_complete_row("not_ct"))
})

# validate_input_get_total_row() ####
test_that("validate_input_get_total_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_total_row(ct, TRUE, "count"))
    expect_error(validate_input_get_total_row("not_ct", TRUE, "count"))
    expect_error(validate_input_get_total_row(ct, "TRUE", "count"))
    expect_error(validate_input_get_total_row(ct, TRUE, 123))
})

# validate_input_add_total_row() ####
test_that("validate_input_add_total_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_total_row(ct))
    expect_error(validate_input_add_total_row("not_ct"))
})

# validate_input_get_complete_total_row() ####
test_that("validate_input_get_complete_total_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_complete_total_row(ct, TRUE, "count"))
    expect_error(validate_input_get_complete_total_row("not_ct", TRUE, "count"))
    expect_error(validate_input_get_complete_total_row(ct, "TRUE", "count"))
    expect_error(validate_input_get_complete_total_row(ct, TRUE, 123))
})

# validate_input_add_complete_total_row() ####
test_that("validate_input_add_complete_total_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_complete_total_row(ct))
    expect_error(validate_input_add_complete_total_row("not_ct"))
})

# validate_input_get_mean_row() ####
test_that("validate_input_get_mean_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_mean_row(ct, FALSE, "mean", 1))
    expect_error(validate_input_get_mean_row("not_ct", FALSE, "mean", 1))
    expect_error(validate_input_get_mean_row(ct, "FALSE", "mean", 1))
    expect_error(validate_input_get_mean_row(ct, FALSE, 999, 1))
    expect_error(validate_input_get_mean_row(ct, FALSE, "mean", "not numeric"))
})

# validate_input_add_mean_row() ####
test_that("validate_input_add_mean_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_mean_row(ct, 2, F, "symbol", F))
    expect_error(validate_input_add_mean_row("not_ct", 2, F, "symbol", F))
    expect_error(validate_input_add_mean_row(ct, "two", F, "symbol", F))
})

# validate_input_get_sd_row() ####
test_that("validate_input_get_sd_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_sd_row(ct, FALSE, "sd", 1))
    expect_error(validate_input_get_sd_row("not_ct", FALSE, "sd", 1))
    expect_error(validate_input_get_sd_row(ct, "FALSE", "sd", 1))
    expect_error(validate_input_get_sd_row(ct, FALSE, 999, 1))
    expect_error(validate_input_get_sd_row(ct, FALSE, "sd", "not numeric"))
})

# validate_input_add_sd_row() ####
test_that("validate_input_add_sd_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_sd_row(ct, 2))
    expect_error(validate_input_add_sd_row("not_ct", 2))
    expect_error(validate_input_add_sd_row(ct, "two"))
})

# validate_input_get_mean_sd_row() ####
test_that("validate_input_get_mean_sd_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_mean_sd_row(ct, FALSE, "mean_sd", 1))
    expect_error(validate_input_get_mean_sd_row("not_ct", FALSE, "mean_sd", 1))
    expect_error(validate_input_get_mean_sd_row(ct, "FALSE", "mean_sd", 1))
    expect_error(validate_input_get_mean_sd_row(ct, FALSE, 999, 1))
    expect_error(validate_input_get_mean_sd_row(ct, FALSE, "mean_sd", "not numeric"))
})

# validate_input_add_mean_sd_row() ####
test_that("validate_input_add_mean_sd_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_mean_sd_row(ct, 2, F, "symbol", F))
    expect_error(validate_input_add_mean_sd_row("not_ct", 2, F, "symbol", F))
    expect_error(validate_input_add_mean_sd_row(ct, "two", F, "symbol", F))
})

# validate_input_get_med_row() ####
test_that("validate_input_get_med_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_med_row(ct, TRUE, "med", 0.5))
    expect_error(validate_input_get_med_row("not_ct", TRUE, "med", 0.5))
    expect_error(validate_input_get_med_row(ct, 1, "med", 0.5))
    expect_error(validate_input_get_med_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_med_row(ct, TRUE, "med", "NaN"))
})

# validate_input_add_med_row() ####
test_that("validate_input_add_med_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_med_row(ct, 3))
    expect_error(validate_input_add_med_row("not_ct", 3))
    expect_error(validate_input_add_med_row(ct, "three"))
})

# validate_input_get_q1_row() ####
test_that("validate_input_get_q1_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_q1_row(ct, TRUE, "q1", 0.5))
    expect_error(validate_input_get_q1_row("not_ct", TRUE, "q1", 0.5))
    expect_error(validate_input_get_q1_row(ct, 1, "q1", 0.5))
    expect_error(validate_input_get_q1_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_q1_row(ct, TRUE, "q1", "NaN"))
})

# validate_input_add_q1_row() ####
test_that("validate_input_add_q1_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_q1_row(ct, 3))
    expect_error(validate_input_add_q1_row("not_ct", 3))
    expect_error(validate_input_add_q1_row(ct, "three"))
})

# validate_input_get_q3_row() ####
test_that("validate_input_get_q3_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_q3_row(ct, TRUE, "q3", 0.5))
    expect_error(validate_input_get_q3_row("not_ct", TRUE, "q3", 0.5))
    expect_error(validate_input_get_q3_row(ct, 1, "q3", 0.5))
    expect_error(validate_input_get_q3_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_q3_row(ct, TRUE, "q3", "NaN"))
})

# validate_input_add_q3_row() ####
test_that("validate_input_add_q3_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_q3_row(ct, 3))
    expect_error(validate_input_add_q3_row("not_ct", 3))
    expect_error(validate_input_add_q3_row(ct, "three"))
})

# validate_input_get_q1_q3_row() ####
test_that("validate_input_get_q1_q3_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_q1_q3_row(ct, TRUE, "q1_q3", 0.5))
    expect_error(validate_input_get_q1_q3_row("not_ct", TRUE, "q1_q3", 0.5))
    expect_error(validate_input_get_q1_q3_row(ct, 1, "q1_q3", 0.5))
    expect_error(validate_input_get_q1_q3_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_q1_q3_row(ct, TRUE, "q1_q3", "NaN"))
})

# validate_input_add_q1_q3_row() ####
test_that("validate_input_add_q1_q3_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_q1_q3_row(ct, 3))
    expect_error(validate_input_add_q1_q3_row("not_ct", 3))
    expect_error(validate_input_add_q1_q3_row(ct, "three"))
})

# validate_input_get_iqr_row() ####
test_that("validate_input_get_iqr_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_iqr_row(ct, TRUE, "iqr", 0.5))
    expect_error(validate_input_get_iqr_row("not_ct", TRUE, "iqr", 0.5))
    expect_error(validate_input_get_iqr_row(ct, 1, "iqr", 0.5))
    expect_error(validate_input_get_iqr_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_iqr_row(ct, TRUE, "iqr", "NaN"))
})

# validate_input_add_iqr_row() ####
test_that("validate_input_add_iqr_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_iqr_row(ct, 3))
    expect_error(validate_input_add_iqr_row("not_ct", 3))
    expect_error(validate_input_add_iqr_row(ct, "three"))
})

# validate_input_get_iqr_q3_q1_row() ####
test_that("validate_input_get_iqr_q3_q1_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_iqr_q3_q1_row(ct, TRUE, "iqr_q3_q1", 0.5))
    expect_error(validate_input_get_iqr_q3_q1_row("not_ct", TRUE, "iqr_q3_q1", 0.5))
    expect_error(validate_input_get_iqr_q3_q1_row(ct, 1, "iqr_q3_q1", 0.5))
    expect_error(validate_input_get_iqr_q3_q1_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_iqr_q3_q1_row(ct, TRUE, "iqr_q3_q1", "NaN"))
})

# validate_input_add_iqr_q3_q1_row() ####
test_that("validate_input_add_iqr_q3_q1_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_iqr_q3_q1_row(ct, 3))
    expect_error(validate_input_add_iqr_q3_q1_row("not_ct", 3))
    expect_error(validate_input_add_iqr_q3_q1_row(ct, "three"))
})

# validate_input_get_med_q1_q3_row() ####
test_that("validate_input_get_med_q1_q3_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_med_q1_q3_row(ct, TRUE, "med_q1_q3", 0.5))
    expect_error(validate_input_get_med_q1_q3_row("not_ct", TRUE, "med_q1_q3", 0.5))
    expect_error(validate_input_get_med_q1_q3_row(ct, 1, "med_q1_q3", 0.5))
    expect_error(validate_input_get_med_q1_q3_row(ct, TRUE, 123, 0.5))
    expect_error(validate_input_get_med_q1_q3_row(ct, TRUE, "med_q1_q3", "NaN"))
})

# validate_input_add_med_q1_q3_row() ####
test_that("validate_input_add_med_q1_q3_row() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_med_q1_q3_row(ct, 3))
    expect_error(validate_input_add_med_q1_q3_row("not_ct", 3))
    expect_error(validate_input_add_med_q1_q3_row(ct, "three"))
})

# validate_input_get_count_rows() ####
test_that("validate_input_get_count_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_count_rows(ct, FALSE, "count"))
    expect_error(validate_input_get_count_rows("not_ct", FALSE, "count"))
    expect_error(validate_input_get_count_rows(ct, "FALSE", "count"))
    expect_error(validate_input_get_count_rows(ct, FALSE, 999))
})

# validate_input_add_count_rows() ####
test_that("validate_input_add_count_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_count_rows(ct))
    expect_error(validate_input_add_count_rows("not_ct"))
})

# validate_input_get_prop_rows() ####
test_that("validate_input_get_prop_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_prop_rows(ct, FALSE, "prop", 0))
    expect_error(validate_input_get_prop_rows("not_ct", FALSE, "prop", 0))
    expect_error(validate_input_get_prop_rows(ct, "FALSE", "prop", 0))
    expect_error(validate_input_get_prop_rows(ct, FALSE, 999, 0))
    expect_error(validate_input_get_prop_rows(ct, FALSE, "prop", "zero"))
})

# validate_input_add_prop_rows() ####
test_that("validate_input_add_prop_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_prop_rows(ct, 4))
    expect_error(validate_input_add_prop_rows("not_ct", 4))
    expect_error(validate_input_add_prop_rows(ct, "four"))
})

# validate_input_get_count_prop_rows() ####
test_that("validate_input_get_count_prop_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_count_prop_rows(ct, FALSE, "count_prop", 0))
    expect_error(validate_input_get_count_prop_rows("not_ct", FALSE, "count_prop", 0))
    expect_error(validate_input_get_count_prop_rows(ct, "FALSE", "count_prop", 0))
    expect_error(validate_input_get_count_prop_rows(ct, FALSE, 999, 0))
    expect_error(validate_input_get_count_prop_rows(ct, FALSE, "count_prop", "zero"))
})

# validate_input_add_count_prop_rows() ####
test_that("validate_input_add_count_prop_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_count_prop_rows(ct, 4))
    expect_error(validate_input_add_count_prop_rows("not_ct", 4))
    expect_error(validate_input_add_count_prop_rows(ct, "four"))
})

# validate_input_get_percent_rows() ####
test_that("validate_input_get_percent_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_percent_rows(ct, FALSE, "percent", 0))
    expect_error(validate_input_get_percent_rows("not_ct", FALSE, "percent", 0))
    expect_error(validate_input_get_percent_rows(ct, "FALSE", "percent", 0))
    expect_error(validate_input_get_percent_rows(ct, FALSE, 999, 0))
    expect_error(validate_input_get_percent_rows(ct, FALSE, "percent", "zero"))
})

# validate_input_add_percent_rows() ####
test_that("validate_input_add_percent_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_percent_rows(ct, 4))
    expect_error(validate_input_add_percent_rows("not_ct", 4))
    expect_error(validate_input_add_percent_rows(ct, "four"))
})

# validate_input_get_count_percent_rows() ####
test_that("validate_input_get_count_percent_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_get_count_percent_rows(ct, FALSE, "count_percent", 0))
    expect_error(validate_input_get_count_percent_rows("not_ct", FALSE, "count_percent", 0))
    expect_error(validate_input_get_count_percent_rows(ct, "FALSE", "count_percent", 0))
    expect_error(validate_input_get_count_percent_rows(ct, FALSE, 999, 0))
    expect_error(validate_input_get_count_percent_rows(ct, FALSE, "count_percent", "zero"))
})

# validate_input_add_count_percent_rows() ####
test_that("validate_input_add_count_percent_rows() works with correct types", {
    ct <- structure(data.frame(x = 1), class = c("crosstab", "data.frame"))
    expect_silent(validate_input_add_count_percent_rows(ct, 4))
    expect_error(validate_input_add_count_percent_rows("not_ct", 4))
    expect_error(validate_input_add_count_percent_rows(ct, "four"))
})
