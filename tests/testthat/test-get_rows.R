# Tests for positive cases. Negative, error-causing cases are checked in the test file for the associated "validate" script.

# to_wide() ####
test_that("to_wide() returns the proper data without error", {
    long_df <- factorize_columns(data.frame(
        description = "test description",
        cohort = c("a", "b", "c"),
        value = c(2, 3, 4)
    ))

    expect_silent(to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    ))

    result <- to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    )

    expect_equal(ncol(result), 4)
    expect_equal(nrow(result), 1)
    expect_true("description" %in% names(result))
    expect_true(all(names(result) %in% c("description", "a", "b", "c")))
    expect_false(any(is.na(result)))
})

test_that("to_wide() returns the proper data without error when NA values are included in the data", {
    long_df <- factorize_columns(data.frame(
        description = "test description",
        cohort = c("a", "b", "c", NA),
        value = c(2, 3, NA, 1)
    ))

    expect_silent(to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    ))

    result <- to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    )

    expect_equal(ncol(result), 5)
    expect_equal(nrow(result), 1)
    expect_true("description" %in% names(result))
    expect_true(all(names(result) %in% c("description", "NA", "a", "b", "c")))
    expect_true(any(is.na(result)))
})

test_that("to_wide() returns the proper data without error when given multiple description values", {
    long_df <- factorize_columns(data.frame(
        description = c("desc 1", "desc 2", "desc 3"),
        cohort = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
        value = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    ))

    expect_silent(to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    ))

    result <- to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    )

    expect_equal(ncol(result), 4)
    expect_equal(nrow(result), 3)
    expect_true("description" %in% names(result))
    expect_true(all(names(result) %in% c("description", "a", "b", "c")))
    expect_false(any(is.na(result)))
})

test_that("to_wide() returns the proper data without error when given multiple description values and data includes NA values", {
    long_df <- factorize_columns(data.frame(
        description = c("desc 1", "desc 2", "desc 3"),
        cohort = c("a", "a", "a", "b", "b", "b", "c", "c", "c", NA, NA, NA),
        value = c(1, 2, 3, 4, NA, 6, 7, 8, 9, NA, 11, NA)
    ))

    expect_silent(to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    ))

    result <- to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort"
    )

    expect_equal(ncol(result), 5)
    expect_equal(nrow(result), 3)
    expect_true("description" %in% names(result))
    expect_true(all(names(result) %in% c("description", "a", "b", "c", "NA")))
    expect_true(any(is.na(result)))
})

test_that("to_wide() properly fills NA values in the output cohort columns", {
    long_df <- factorize_columns(data.frame(
        description = c("desc 1", "desc 2", "desc 3"),
        cohort = c("a", "a", "a", "b", "b", "b", "c", "c", "c", NA, NA, NA),
        value = c(1, 2, 3, 4, NA, 6, 7, 8, 9, NA, 11, NA)
    ))

    expect_silent(to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort",
        na_fill = "THIS WAS NA"
    ))

    result <- to_wide(
        long_df = long_df,
        desc_col = "description",
        cohort_col = "cohort",
        na_fill = "THIS WAS NA"
    )

    expect_equal(ncol(result), 5)
    expect_equal(nrow(result), 3)
    expect_true("description" %in% names(result))
    expect_true(all(names(result) %in% c("description", "a", "b", "c", "NA")))
    expect_false(any(is.na(result)))
    expect_true(any(sapply(result, function(col) "THIS WAS NA" %in% col)))
})

# to_long() ####
test_that("to_long() returns the proper data without errors",{
    wide_df <- factorize_columns(data.frame(
        description = "test description",
        a = 1,
        b = 2,
        c = 3
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 3)
    expect_false(any(is.na(result)))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c")))
})

test_that("to_long() returns the proper data without errors when data includes NA values",{
    wide_df <- factorize_columns(data.frame(
        description = "test description",
        a = 1,
        b = 2,
        c = as.double(NA),
        "NA" = 4,
        check.names = FALSE
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 4)
    expect_true(any(is.na(result[["cohort"]])))
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c", NA)))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
})

test_that("to_long() returns the proper data without errors when given multi-row data",{
    wide_df <- factorize_columns(data.frame(
        description = c("test 1", "test 2", "test 3"),
        a = c(1, 2, 3),
        b = c(4, 5, 6),
        c = c(7, 8, 9)
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 9)
    expect_false(any(is.na(result)))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c")))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
})

test_that("to_long() returns the proper data without errors when given multi-row data with duplicate descriptions",{
    wide_df <- factorize_columns(data.frame(
        description = "test 1",
        a = c(1, 2, 3),
        b = c(4, 5, 6),
        c = c(7, 8, 9)
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 9)
    expect_false(any(is.na(result)))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c")))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
    expect_equal(length(unique(result[["description"]])), 1)
})

test_that("to_long() returns the proper data without errors when given multi-row data, with NA values",{
    wide_df <- factorize_columns(data.frame(
        description = c("test 1", NA, "test 3"),
        a = c(1, NA, 3),
        b = c(4, 5, NA),
        c = c(7, NA, 9),
        "NA" = c(10, NA, 12),
        check.names = F
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 12)
    expect_true(any(is.na(result[["description"]])))
    expect_true(any(is.na(result[["cohort"]])))
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c", NA)))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
})

test_that("to_long() returns the proper data without errors when given multi-row data, with duplicate descriptions and NA values",{
    wide_df <- factorize_columns(data.frame(
        description = c("test 1"),
        a = c(1, NA, 3),
        b = c(4, 5, NA),
        c = c(7, NA, 9),
        "NA" = c(10, NA, 12),
        check.names = F
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value"
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 12)
    expect_true(any(is.na(result[["cohort"]])))
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c", NA)))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
})

test_that("to_long() returns the proper data when convert_NA_cohort is false",{
    wide_df <- factorize_columns(data.frame(
        description = c("test 1", NA, "test 3"),
        a = c(1, NA, 3),
        b = c(4, 5, NA),
        c = c(7, NA, 9),
        "NA" = c(10, NA, 12),
        check.names = F
    ))

    expect_silent(to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value",
        convert_NA_cohort = F
    ))

    result <- to_long(
        wide_df = wide_df,
        description_col = "description",
        cohorts_to = "cohort",
        values_to = "value",
        convert_NA_cohort = F
    )

    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 12)
    expect_false(any(is.na(result[["cohort"]])))
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c", "NA")))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
})

# add_rows() ####
test_that("add_rows() works when adding a row to an empty table",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    new_rows <- data.frame(
        Description = "test Description",
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    expect_silent(add_rows(ct, new_rows))
    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 1)
    expect_false(any(is.na(ct)))
})

test_that("add_rows() updates index correctly",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    new_rows <- data.frame(
        Description = "test Description",
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    ct <- add_rows(ct, new_rows)

    expect_equal(index(ct), c(variable = 1))

    new_rows <- data.frame(
        Description = c("test Description 1", "test Description 2", "test Description 3"),
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    ct <- add_rows(ct, new_rows)

    expect_equal(index(ct), c(variable = 4))
})

test_that("add_rows() works when adding a row to a table that already has rows with matching columns",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    new_rows <- data.frame(
        Description = "test Description",
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 1)
    expect_false(any(is.na(ct)))

    expect_silent(add_rows(ct, new_rows))
    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 2)
    expect_false(any(is.na(ct)))
})

test_that("add_rows() works when adding multiple rows to an empty table",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    new_rows <- data.frame(
        Description = "test Description",
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    new_rows <- rbind(new_rows, new_rows)

    expect_silent(add_rows(ct, new_rows))
    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 2)
    expect_false(any(is.na(ct)))
})

test_that("add_rows() works when adding mmultiple rows to a table that already has rows with matching columns",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    new_rows <- data.frame(
        Description = "test Description",
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 1)
    expect_false(any(is.na(ct)))

    new_rows <- rbind(new_rows, new_rows)

    expect_silent(add_rows(ct, new_rows))
    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 3)
    expect_false(any(is.na(ct)))
})

test_that("add_rows() warns when adding rows with columns that don't match existing columns",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    new_rows <- data.frame(
        Description = "test Description",
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        "NA" = "test NA",
        check.names = F
    )

    expect_silent(add_rows(ct, new_rows))
    ct <- add_rows(ct, new_rows)

    expect_equal(ncol(ct), 7)
    expect_equal(nrow(ct), 1)
    expect_false(any(is.na(ct)))

    new_row_special <- data.frame(
        new_col = "new val"
    )

    expect_warning(add_rows(ct, new_row_special))
    ct <- suppressWarnings(add_rows(ct, new_row_special))

    expect_equal(ncol(ct), 8)
    expect_equal(nrow(ct), 2)
    expect_true(any(is.na(ct)))
    expect_true(all(sapply(ct, function(col) any(is.na(col)))))
})

# get_total_row() ####
test_that("get_total_row() works given proper data",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_total_row(ct))
    total_row <- get_total_row(ct)

    expect_equal(nrow(total_row), 1)
    expect_equal(ncol(total_row), 7)
    expect_false(any(is.na(total_row)))
    expect_true(all(names(total_row) %in% c("Description", "All", "NA", character_levels)))
})

test_that("get_total_row() works and long_out_col has no effect on the output if long = T",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_total_row(ct))
    total_row_1 <- get_total_row(ct)
    total_row_2 <- get_total_row(ct, long_out_col = "test")

    expect_identical(total_row_2, total_row_1)
})

test_that("get_total_row() works with long = T",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_total_row(ct, long = T))
    total_long <- get_total_row(ct, long = T)

    expect_equal(nrow(total_long), 6)
    expect_equal(ncol(total_long), 3)
    expect_true(any(is.na(total_long[["cohort"]])))
    expect_true(all(total_long[["cohort"]] %in% c("All", NA, character_levels)))
})

test_that("get_total_row() works with long = T, long_out_col changes the column name",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_total_row(ct, long = T, long_out_col = "This is a test column name"))
    total_long <- get_total_row(ct, long = T, long_out_col = "This is a test column name")

    expect_in("This is a test column name", names(total_long))

    expect_equal(nrow(total_long), 6)
    expect_equal(ncol(total_long), 3)
    expect_true(any(is.na(total_long[["cohort"]])))
    expect_true(all(total_long[["cohort"]] %in% c("All", NA, character_levels)))
})

# add_total_row() ####
test_that("add_total_row() works with proper data",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_total_row(ct))

    ct <- add_total_row(ct)

    expect_equal(nrow(ct), 1)
    expect_equal(ncol(ct), 7)

    ct <- add_total_row(ct)

    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 7)
})

# get_mean_sd_row() ####
test_that("get_mean_sd_row() works given proper data", {
    ct <- crosstab(
        df = num_test_df(),  # assumes numeric data; switch if needed
        cohort_col_name = "cohort"
    )

    expect_silent(get_mean_sd_row(ct))
    mean_sd_row <- get_mean_sd_row(ct)

    expect_equal(nrow(mean_sd_row), 1)
    expect_true(ncol(mean_sd_row) >= 3)  # at least desc, cohort, and one data col
    expect_false(any(is.na(mean_sd_row)))
    expect_true("Description" %in% names(mean_sd_row))
})

test_that("get_mean_sd_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_mean_sd_row(ct)
    row2 <- get_mean_sd_row(ct, long_out_col = "TestColumn")

    expect_identical(row1, row2)
})

test_that("get_mean_sd_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_mean_sd_row(ct, long = T))
    long <- get_mean_sd_row(ct, long = T)

    expect_true(nrow(long) >= 3)
    expect_equal(ncol(long), 3)
    expect_true("cohort" %in% names(long))
    expect_true(any(is.na(long[["cohort"]])))
    expect_true(all(c("Description") %in% names(long)))
})

test_that("get_mean_sd_row() allows custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_name <- "test"
    long <- get_mean_sd_row(ct, long = T, long_out_col = custom_name)

    expect_in(custom_name, names(long))
    expect_equal(ncol(long), 3)
})

test_that("get_mean_sd_row() obeys rounding parameter", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row_1dp <- get_mean_sd_row(ct, round_to = 1, long = T)
    row_3dp <- get_mean_sd_row(ct, round_to = 3, long = T)

    # Values should not be identical due to different rounding
    expect_false(identical(row_1dp[[3]], row_3dp[[3]]))
})

# add_mean_sd_row() ####
test_that("add_mean_sd_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_mean_sd_row(ct))

    ct <- add_mean_sd_row(ct)

    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_mean_sd_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_mean_sd_row() respects round_to argument", {
    seed <- 1234
    ct1 <- crosstab(df = num_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = num_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_mean_sd_row(ct1, round_to = 0)
    ct2 <- add_mean_sd_row(ct2, round_to = 3)

    # Should differ due to rounding
    expect_false(identical(ct1, ct2))
})

# get_med_iqr_row() ####
test_that("get_med_iqr_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_med_iqr_row(ct))
    med_iqr_row <- get_med_iqr_row(ct)

    expect_equal(nrow(med_iqr_row), 1)
    expect_true(ncol(med_iqr_row) >= 3)
    expect_false(any(is.na(med_iqr_row)))
    expect_true("Description" %in% names(med_iqr_row))
})

test_that("get_med_iqr_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_med_iqr_row(ct)
    row2 <- get_med_iqr_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_med_iqr_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_med_iqr_row(ct, long = T))
    med_iqr_long <- get_med_iqr_row(ct, long = T)

    expect_true(nrow(med_iqr_long) >= 3)
    expect_equal(ncol(med_iqr_long), 3)
    expect_true("cohort" %in% names(med_iqr_long))
    expect_true("Description" %in% names(med_iqr_long))
    expect_true(any(is.na(med_iqr_long[["cohort"]])))
})

test_that("get_med_iqr_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    med_iqr_long <- get_med_iqr_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(med_iqr_long))
    expect_equal(ncol(med_iqr_long), 3)
})

test_that("get_med_iqr_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_med_iqr_row(ct1, round_to = 0, long = T)
    row2 <- get_med_iqr_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_med_iqr_row() ####
test_that("add_med_iqr_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_med_iqr_row(ct))

    ct <- add_med_iqr_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_med_iqr_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_med_iqr_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_med_iqr_row(ct1, round_to = 0)
    ct2 <- add_med_iqr_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_count_rows() ####
test_that("get_count_rows() works with proper categorical data", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_count_rows(ct))
    count_rows <- get_count_rows(ct)

    expect_true(nrow(count_rows) >= 2)
    expect_true(ncol(count_rows) >= 3)

    expect_true("Description" %in% names(count_rows))

    cohort_col_names <- setdiff(names(count_rows), "Description")
    expect_false(any(is.na(count_rows[, cohort_col_names])))
})

test_that("get_count_rows() long = F ignores long_out_col argument", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_count_rows(ct)
    row2 <- get_count_rows(ct, long_out_col = "my_custom_count_col")

    expect_identical(row1, row2)
})

test_that("get_count_rows() works with long = T and custom long_out_col", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_count_rows(ct, long = T, long_out_col = "count_percent"))
    count_long <- get_count_rows(ct, long = T, long_out_col = "count_percent")

    expect_equal(ncol(count_long), 3)
    expect_in("count_percent", names(count_long))
    expect_in("Description", names(count_long))
    expect_in("cohort", names(count_long))
})

test_that("get_count_rows() produces 0% output when na_fill kicks in", {
    test_df <- cat_test_df()

    keep <- which(
        !(test_df[["variable"]] == categorical_levels[1] &
        test_df[["cohort"]] == character_levels[1])
    )

    test_df <- test_df[keep,]

    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    wide_out <- get_count_rows(ct, long = T)
    expect_true(any(wide_out == "0 (0%)"))
})

# add_count_rows() ####
test_that("add_count_rows() adds correct number of rows", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    ct <- add_count_rows(ct)
    expect_gt(nrow(ct), 0)

    row_count_after_first <- nrow(ct)

    ct <- add_count_rows(ct)
    expect_equal(nrow(ct), 2 * row_count_after_first)
})

test_that("add_count_rows() respects round_to", {
    seed <- 4321
    ct1 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_count_rows(ct1, round_to = 0)
    ct2 <- add_count_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})
