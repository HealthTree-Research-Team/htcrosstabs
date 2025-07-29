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
    expect_equal(nrow(result), 3)
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c")))
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

test_that("to_long() returns the proper data without errors when given multi-row data and NA values",{
    wide_df <- factorize_columns(data.frame(
        description = c("test 1", NA, "test 3"),
        a = c(1, NA, 3),
        b = c(4, 5, NA),
        c = c(7, NA, 9),
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
    expect_equal(nrow(result), 9)
    expect_true(any(is.na(result[["description"]])))
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c")))
    expect_true(setequal(wide_df[["description"]], result[["description"]]))
    expect_true(is.numeric(result[["value"]]))
})

test_that("to_long() returns the proper data without errors when given multi-row data, with duplicate descriptions and NA values",{
    wide_df <- factorize_columns(data.frame(
        description = c("test 1"),
        a = c(1, NA, 3),
        b = c(4, 5, NA),
        c = c(7, NA, 9),
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
    expect_equal(nrow(result), 9)
    expect_true(any(is.na(result[["value"]])))
    expect_true(all(result[["cohort"]] %in% c("a", "b", "c")))
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

test_that("add_rows() adds the row in the proper spot when ind is provided",{
    ct <- crosstab(cat_test_df(col_name = "tab1"), "cohort") |> add_default_table(anova = F, chisq = F)
    new_ct <- crosstab(num_test_df(col_name = "tab2"), "cohort") |> add_default_table(anova = F, chisq = F)
    ct <- stack_crosstabs(ct, new_ct)

    new_rows <- data.frame(
        Description = c("desc1", "desc2", "desc3"),
        All = "test All",
        A = "test A",
        B = "test B",
        C = "test C",
        D = "test D",
        check.names = F
    )

    expect_silent(add_rows(ct, new_rows, ind = 1, table_name = "new_table"))
    result <- add_rows(ct, new_rows, ind = 1, table_name = "new_table")
    expect_equal(nrow(result), 11)
    expect_equal(result[1:3, , drop = F] |> data.frame(check.names = F), new_rows)

    expect_equal(sum(index(ct)) + 3, sum(index(result)))

    expect_silent(add_rows(ct, new_rows, ind = NULL, index_from = "bottom", table_name = "new_table"))
    result <- add_rows(ct, new_rows, ind = NULL, index_from = "bottom", table_name = "new_table")
    expect_equal(nrow(result), 11)
    expect_equal(result[1:3, , drop = F] |> data.frame(check.names = F), new_rows)

    expect_equal(sum(index(ct)) + 3, sum(index(result)))

    expect_silent(add_rows(ct, new_rows, ind = 10, table_name = "new_table"))
    result <- add_rows(ct, new_rows, ind = 10, table_name = "new_table")
    expect_equal(nrow(result), 11)
    expect_equal(result[9:11, , drop = F] |> data.frame(check.names = F, row.names = NULL), new_rows)

    expect_equal(sum(index(ct)) + 3, sum(index(result)))

    expect_silent(add_rows(ct, new_rows, ind = 1, index_from = "bottom"))
    result <- add_rows(ct, new_rows, ind = 1, index_from = "bottom")
    expect_equal(nrow(result), 11)
    expect_equal(result[9:11, , drop = F] |> data.frame(check.names = F, row.names = NULL), new_rows)

    expect_equal(sum(index(ct)) + 3, sum(index(result)))
})

# get_complete_row() ####
test_that("get_complete_row() works given proper data",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_row(ct))
    complete_row <- get_complete_row(ct)

    expect_equal(nrow(complete_row), 1)
    expect_equal(ncol(complete_row), 6)
    expect_false(any(is.na(complete_row)))
    expect_true(all(names(complete_row) %in% c("Description", "All", character_levels)))
})

test_that("get_complete_row() works and long_out_col has no effect on the output if long = T",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_row(ct))
    complete_row_1 <- get_complete_row(ct)
    complete_row_2 <- get_complete_row(ct, long_out_col = "test")

    expect_identical(complete_row_2, complete_row_1)
})

test_that("get_complete_row() works with long = T",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_row(ct, long = T))
    total_long <- get_complete_row(ct, long = T)

    expect_equal(nrow(total_long), 5)
    expect_equal(ncol(total_long), 3)
    expect_true(all(total_long[["cohort"]] %in% c("All", character_levels)))
})

test_that("get_complete_row() works with long = T, long_out_col changes the column name",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_row(ct, long = T, long_out_col = "This is a test column name"))
    total_long <- get_complete_row(ct, long = T, long_out_col = "This is a test column name")

    expect_in("This is a test column name", names(total_long))

    expect_equal(nrow(total_long), 5)
    expect_equal(ncol(total_long), 3)
    expect_true(all(total_long[["cohort"]] %in% c("All", character_levels)))
})

# add_complete_row() ####
test_that("add_complete_row() works with proper data",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_complete_row(ct))

    ct <- add_complete_row(ct)

    expect_equal(nrow(ct), 1)
    expect_equal(ncol(ct), 6)

    ct <- add_complete_row(ct)

    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 6)
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
    expect_equal(ncol(total_row), 6)
    expect_false(any(is.na(total_row)))
    expect_true(all(names(total_row) %in% c("Description", "All", character_levels)))
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

    expect_equal(nrow(total_long), 5)
    expect_equal(ncol(total_long), 3)
    expect_true(all(total_long[["cohort"]] %in% c("All", character_levels)))
})

test_that("get_total_row() works with long = T, long_out_col changes the column name",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_total_row(ct, long = T, long_out_col = "This is a test column name"))
    total_long <- get_total_row(ct, long = T, long_out_col = "This is a test column name")

    expect_in("This is a test column name", names(total_long))

    expect_equal(nrow(total_long), 5)
    expect_equal(ncol(total_long), 3)
    expect_true(all(total_long[["cohort"]] %in% c("All", character_levels)))
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
    expect_equal(ncol(ct), 6)

    ct <- add_total_row(ct)

    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 6)
})

# get_complete_total_row() ####
test_that("get_complete_total_row() works given proper data",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_total_row(ct))
    complete_total_row <- get_complete_total_row(ct)

    expect_equal(nrow(complete_total_row), 1)
    expect_equal(ncol(complete_total_row), 6)
    expect_true(all(names(complete_total_row) %in% c("Description", "All", character_levels)))
})

test_that("get_complete_total_row() works and long_out_col has no effect on the output if long = T",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_total_row(ct))
    complete_total_row_1 <- get_complete_total_row(ct)
    complete_total_row_2 <- get_complete_total_row(ct, long_out_col = "test")

    expect_identical(complete_total_row_2, complete_total_row_1)
})

test_that("get_complete_total_row() works with long = T",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_total_row(ct, long = T))
    total_long <- get_complete_total_row(ct, long = T)

    expect_equal(nrow(total_long), 5)
    expect_equal(ncol(total_long), 3)
    expect_true(all(total_long[["cohort"]] %in% c("All", character_levels)))
})

test_that("get_complete_total_row() works with long = T, long_out_col changes the column name",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_complete_total_row(ct, long = T, long_out_col = "This is a test column name"))
    total_long <- get_complete_total_row(ct, long = T, long_out_col = "This is a test column name")

    expect_in("This is a test column name", names(total_long))

    expect_equal(nrow(total_long), 5)
    expect_equal(ncol(total_long), 3)
    expect_true(all(total_long[["cohort"]] %in% c("All", character_levels)))
})

# add_complete_total_row() ####
test_that("add_complete_total_row() works with proper data",{
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_complete_total_row(ct))

    ct <- add_complete_total_row(ct)

    expect_equal(nrow(ct), 1)
    expect_equal(ncol(ct), 6)

    ct <- add_complete_total_row(ct)

    expect_equal(nrow(ct), 2)
    expect_equal(ncol(ct), 6)
})

# get_mean_row() ####
test_that("get_mean_row() works given proper data", {
    ct <- crosstab(
        df = num_test_df(),  # assumes numeric data; switch if needed
        cohort_col_name = "cohort"
    )

    expect_silent(get_mean_row(ct))
    mean_row <- get_mean_row(ct)

    expect_equal(nrow(mean_row), 1)
    expect_true(ncol(mean_row) >= 3)  # at least desc, cohort, and one data col
    expect_false(any(is.na(mean_row)))
    expect_true("Description" %in% names(mean_row))
})

test_that("get_mean_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_mean_row(ct)
    row2 <- get_mean_row(ct, long_out_col = "TestColumn")

    expect_identical(row1, row2)
})

test_that("get_mean_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_mean_row(ct, long = T))
    long <- get_mean_row(ct, long = T)

    expect_true(nrow(long) >= 3)
    expect_equal(ncol(long), 3)
    expect_true("cohort" %in% names(long))
    expect_true(all(c("Description") %in% names(long)))
})

test_that("get_mean_row() allows custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_name <- "test"
    long <- get_mean_row(ct, long = T, long_out_col = custom_name)

    expect_in(custom_name, names(long))
    expect_equal(ncol(long), 3)
})

test_that("get_mean_row() obeys rounding parameter", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row_1dp <- get_mean_row(ct, round_to = 1, long = T)
    row_3dp <- get_mean_row(ct, round_to = 3, long = T)

    # Values should not be identical due to different rounding
    expect_false(identical(row_1dp[[3]], row_3dp[[3]]))
})

# add_mean_row() ####
test_that("add_mean_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_mean_row(ct))

    ct <- add_mean_row(ct)

    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_mean_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_mean_row() respects round_to argument", {
    seed <- 1234
    ct1 <- crosstab(df = num_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = num_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_mean_row(ct1, round_to = 0)
    ct2 <- add_mean_row(ct2, round_to = 3)

    # Should differ due to rounding
    expect_false(identical(ct1, ct2))
})

# get_sd_row() ####
test_that("get_sd_row() works given proper data", {
    ct <- crosstab(
        df = num_test_df(),  # assumes numeric data; switch if needed
        cohort_col_name = "cohort"
    )

    expect_silent(get_sd_row(ct))
    sd_row <- get_sd_row(ct)

    expect_equal(nrow(sd_row), 1)
    expect_true(ncol(sd_row) >= 3)  # at least desc, cohort, and one data col
    expect_true("Description" %in% names(sd_row))
})

test_that("get_sd_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_sd_row(ct)
    row2 <- get_sd_row(ct, long_out_col = "TestColumn")

    expect_identical(row1, row2)
})

test_that("get_sd_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_sd_row(ct, long = T))
    long <- get_sd_row(ct, long = T)

    expect_true(nrow(long) >= 3)
    expect_equal(ncol(long), 3)
    expect_true("cohort" %in% names(long))
    expect_true(all(c("Description") %in% names(long)))
})

test_that("get_sd_row() allows custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_name <- "test"
    long <- get_sd_row(ct, long = T, long_out_col = custom_name)

    expect_in(custom_name, names(long))
    expect_equal(ncol(long), 3)
})

test_that("get_sd_row() obeys rounding parameter", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row_1dp <- get_sd_row(ct, round_to = 1, long = T)
    row_3dp <- get_sd_row(ct, round_to = 3, long = T)

    # Values should not be identical due to different rounding
    expect_false(identical(row_1dp[[3]], row_3dp[[3]]))
})

# add_sd_row() ####
test_that("add_sd_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_sd_row(ct))

    ct <- add_sd_row(ct)

    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_sd_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_sd_row() respects round_to argument", {
    seed <- 1234
    ct1 <- crosstab(df = num_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = num_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_sd_row(ct1, round_to = 0)
    ct2 <- add_sd_row(ct2, round_to = 3)

    # Should differ due to rounding
    expect_false(identical(ct1, ct2))
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

# get_med_row() ####
test_that("get_med_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_med_row(ct))
    med_row <- get_med_row(ct)

    expect_equal(nrow(med_row), 1)
    expect_true(ncol(med_row) >= 3)
    expect_true("Description" %in% names(med_row))
})

test_that("get_med_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_med_row(ct)
    row2 <- get_med_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_med_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_med_row(ct, long = T))
    med_long <- get_med_row(ct, long = T)

    expect_true(nrow(med_long) >= 3)
    expect_equal(ncol(med_long), 3)
    expect_true("cohort" %in% names(med_long))
    expect_true("Description" %in% names(med_long))
})

test_that("get_med_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    med_long <- get_med_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(med_long))
    expect_equal(ncol(med_long), 3)
})

test_that("get_med_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_med_row(ct1, round_to = 0, long = T)
    row2 <- get_med_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_med_row() ####
test_that("add_med_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_med_row(ct))

    ct <- add_med_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_med_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_med_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_med_row(ct1, round_to = 0)
    ct2 <- add_med_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_q1_row() ####
test_that("get_q1_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_q1_row(ct))
    q1_row <- get_q1_row(ct)

    expect_equal(nrow(q1_row), 1)
    expect_true(ncol(q1_row) >= 3)
    expect_true("Description" %in% names(q1_row))
})

test_that("get_q1_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_q1_row(ct)
    row2 <- get_q1_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_q1_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_q1_row(ct, long = T))
    q1_long <- get_q1_row(ct, long = T)

    expect_true(nrow(q1_long) >= 3)
    expect_equal(ncol(q1_long), 3)
    expect_true("cohort" %in% names(q1_long))
    expect_true("Description" %in% names(q1_long))
})

test_that("get_q1_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    q1_long <- get_q1_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(q1_long))
    expect_equal(ncol(q1_long), 3)
})

test_that("get_q1_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_q1_row(ct1, round_to = 0, long = T)
    row2 <- get_q1_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_q1_row() ####
test_that("add_q1_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_q1_row(ct))

    ct <- add_q1_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_q1_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_q1_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_q1_row(ct1, round_to = 0)
    ct2 <- add_q1_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_q3_row() ####
test_that("get_q3_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_q3_row(ct))
    q3_row <- get_q3_row(ct)

    expect_equal(nrow(q3_row), 1)
    expect_true(ncol(q3_row) >= 3)
    expect_true("Description" %in% names(q3_row))
})

test_that("get_q3_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_q3_row(ct)
    row2 <- get_q3_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_q3_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_q3_row(ct, long = T))
    q3_long <- get_q3_row(ct, long = T)

    expect_true(nrow(q3_long) >= 3)
    expect_equal(ncol(q3_long), 3)
    expect_true("cohort" %in% names(q3_long))
    expect_true("Description" %in% names(q3_long))
})

test_that("get_q3_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    q3_long <- get_q3_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(q3_long))
    expect_equal(ncol(q3_long), 3)
})

test_that("get_q3_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_q3_row(ct1, round_to = 0, long = T)
    row2 <- get_q3_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_q3_row() ####
test_that("add_q3_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_q3_row(ct))

    ct <- add_q3_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_q3_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_q3_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_q3_row(ct1, round_to = 0)
    ct2 <- add_q3_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_q1_q3_row() ####
test_that("get_q1_q3_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_q1_q3_row(ct))
    q1_q3_row <- get_q1_q3_row(ct)

    expect_equal(nrow(q1_q3_row), 1)
    expect_true(ncol(q1_q3_row) >= 3)
    expect_true("Description" %in% names(q1_q3_row))
})

test_that("get_q1_q3_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_q1_q3_row(ct)
    row2 <- get_q1_q3_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_q1_q3_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_q1_q3_row(ct, long = T))
    q1_q3_long <- get_q1_q3_row(ct, long = T)

    expect_true(nrow(q1_q3_long) >= 3)
    expect_equal(ncol(q1_q3_long), 3)
    expect_true("cohort" %in% names(q1_q3_long))
    expect_true("Description" %in% names(q1_q3_long))
})

test_that("get_q1_q3_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    q1_q3_long <- get_q1_q3_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(q1_q3_long))
    expect_equal(ncol(q1_q3_long), 3)
})

test_that("get_q1_q3_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_q1_q3_row(ct1, round_to = 0, long = T)
    row2 <- get_q1_q3_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_q1_q3_row() ####
test_that("add_q1_q3_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_q1_q3_row(ct))

    ct <- add_q1_q3_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_q1_q3_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_q1_q3_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_q1_q3_row(ct1, round_to = 0)
    ct2 <- add_q1_q3_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_iqr_row() ####
test_that("get_iqr_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_iqr_row(ct))
    iqr_row <- get_iqr_row(ct)

    expect_equal(nrow(iqr_row), 1)
    expect_true(ncol(iqr_row) >= 3)
    expect_true("Description" %in% names(iqr_row))
})

test_that("get_iqr_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_iqr_row(ct)
    row2 <- get_iqr_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_iqr_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_iqr_row(ct, long = T))
    iqr_long <- get_iqr_row(ct, long = T)

    expect_true(nrow(iqr_long) >= 3)
    expect_equal(ncol(iqr_long), 3)
    expect_true("cohort" %in% names(iqr_long))
    expect_true("Description" %in% names(iqr_long))
})

test_that("get_iqr_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    iqr_long <- get_iqr_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(iqr_long))
    expect_equal(ncol(iqr_long), 3)
})

test_that("get_iqr_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_iqr_row(ct1, round_to = 0, long = T)
    row2 <- get_iqr_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_iqr_row() ####
test_that("add_iqr_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_iqr_row(ct))

    ct <- add_iqr_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_iqr_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_iqr_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_iqr_row(ct1, round_to = 0)
    ct2 <- add_iqr_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_iqr_q3_q1_row() ####
test_that("get_iqr_q3_q1_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_iqr_q3_q1_row(ct))
    iqr_q3_q1_row <- get_iqr_q3_q1_row(ct)

    expect_equal(nrow(iqr_q3_q1_row), 1)
    expect_true(ncol(iqr_q3_q1_row) >= 3)
    expect_true("Description" %in% names(iqr_q3_q1_row))
})

test_that("get_iqr_q3_q1_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_iqr_q3_q1_row(ct)
    row2 <- get_iqr_q3_q1_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_iqr_q3_q1_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_iqr_q3_q1_row(ct, long = T))
    iqr_q3_q1_long <- get_iqr_q3_q1_row(ct, long = T)

    expect_true(nrow(iqr_q3_q1_long) >= 3)
    expect_equal(ncol(iqr_q3_q1_long), 3)
    expect_true("cohort" %in% names(iqr_q3_q1_long))
    expect_true("Description" %in% names(iqr_q3_q1_long))
})

test_that("get_iqr_q3_q1_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    iqr_q3_q1_long <- get_iqr_q3_q1_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(iqr_q3_q1_long))
    expect_equal(ncol(iqr_q3_q1_long), 3)
})

test_that("get_iqr_q3_q1_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_iqr_q3_q1_row(ct1, round_to = 0, long = T)
    row2 <- get_iqr_q3_q1_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_iqr_q3_q1_row() ####
test_that("add_iqr_q3_q1_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_iqr_q3_q1_row(ct))

    ct <- add_iqr_q3_q1_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_iqr_q3_q1_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_iqr_q3_q1_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_iqr_q3_q1_row(ct1, round_to = 0)
    ct2 <- add_iqr_q3_q1_row(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_med_q1_q3_row() ####
test_that("get_med_q1_q3_row() works with proper data", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_med_q1_q3_row(ct))
    med_q1_q3_row <- get_med_q1_q3_row(ct)

    expect_equal(nrow(med_q1_q3_row), 1)
    expect_true(ncol(med_q1_q3_row) >= 3)
    expect_true("Description" %in% names(med_q1_q3_row))
})

test_that("get_med_q1_q3_row() ignores long_out_col when long = F", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_med_q1_q3_row(ct)
    row2 <- get_med_q1_q3_row(ct, long_out_col = "CustomColumnName")

    expect_identical(row1, row2)
})

test_that("get_med_q1_q3_row() works with long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_med_q1_q3_row(ct, long = T))
    med_q1_q3_long <- get_med_q1_q3_row(ct, long = T)

    expect_true(nrow(med_q1_q3_long) >= 3)
    expect_equal(ncol(med_q1_q3_long), 3)
    expect_true("cohort" %in% names(med_q1_q3_long))
    expect_true("Description" %in% names(med_q1_q3_long))
})

test_that("get_med_q1_q3_row() uses custom long_out_col when long = T", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    custom_col <- "test"
    med_q1_q3_long <- get_med_q1_q3_row(ct, long = T, long_out_col = custom_col)

    expect_in(custom_col, names(med_q1_q3_long))
    expect_equal(ncol(med_q1_q3_long), 3)
})

test_that("get_med_q1_q3_row() obeys rounding", {
    seed <- 42

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    row1 <- get_med_q1_q3_row(ct1, round_to = 0, long = T)
    row2 <- get_med_q1_q3_row(ct2, round_to = 3, long = T)

    expect_false(identical(row1[[3]], row2[[3]]))
})

# add_med_q1_q3_row() ####
test_that("add_med_q1_q3_row() adds the row correctly", {
    ct <- crosstab(
        df = num_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_silent(add_med_q1_q3_row(ct))

    ct <- add_med_q1_q3_row(ct)
    expect_equal(nrow(ct), 1)
    expect_true(ncol(ct) >= 3)

    ct <- add_med_q1_q3_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("add_med_q1_q3_row() respects round_to argument", {
    seed <- 5678

    test_df <- num_test_df(seed = seed)
    test_df[["variable"]] <- test_df[["variable"]] / 3

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_med_q1_q3_row(ct1, round_to = 0)
    ct2 <- add_med_q1_q3_row(ct2, round_to = 3)

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

    expect_silent(get_count_rows(ct, long = T, long_out_col = "percent"))
    count_long <- get_count_rows(ct, long = T, long_out_col = "percent")

    expect_equal(ncol(count_long), 3)
    expect_in("percent", names(count_long))
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

    wide_out <- get_count_rows(ct, long = F)
    expect_true(any(wide_out == 0))
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

# get_prop_rows() ####
test_that("get_prop_rows() works with proper categorical data", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_prop_rows(ct))
    prop_rows <- get_prop_rows(ct)

    expect_true(nrow(prop_rows) >= 2)
    expect_true(ncol(prop_rows) >= 3)

    expect_true("Description" %in% names(prop_rows))

    cohort_col_names <- setdiff(names(prop_rows), "Description")
})

test_that("get_prop_rows() long = F ignores long_out_col argument", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_prop_rows(ct)
    row2 <- get_prop_rows(ct, long_out_col = "my_custom_prop_col")

    expect_identical(row1, row2)
})

test_that("get_prop_rows() works with long = T and custom long_out_col", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_prop_rows(ct, long = T, long_out_col = "prop_prop"))
    prop_long <- get_prop_rows(ct, long = T, long_out_col = "prop_prop")

    expect_equal(ncol(prop_long), 3)
    expect_in("prop_prop", names(prop_long))
    expect_in("Description", names(prop_long))
    expect_in("cohort", names(prop_long))
})

test_that("get_prop_rows() produces 0% output when na_fill kicks in", {
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

    wide_out <- get_prop_rows(ct, long = F)
    expect_true(any(wide_out == 0))
})

# add_prop_rows() ####
test_that("add_prop_rows() adds correct number of rows", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    ct <- add_prop_rows(ct)
    expect_gt(nrow(ct), 0)

    row_prop_after_first <- nrow(ct)

    ct <- add_prop_rows(ct)
    expect_equal(nrow(ct), 2 * row_prop_after_first)
})

test_that("add_prop_rows() respects round_to", {
    seed <- 4321
    ct1 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_prop_rows(ct1, round_to = 0)
    ct2 <- add_prop_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_count_prop_rows() ####
test_that("get_count_prop_rows() works with proper categorical data", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_count_prop_rows(ct))
    count_prop_rows <- get_count_prop_rows(ct)

    expect_true(nrow(count_prop_rows) >= 2)
    expect_true(ncol(count_prop_rows) >= 3)

    expect_true("Description" %in% names(count_prop_rows))

    cohort_col_names <- setdiff(names(count_prop_rows), "Description")
})

test_that("get_count_prop_rows() long = F ignores long_out_col argument", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_count_prop_rows(ct)
    row2 <- get_count_prop_rows(ct, long_out_col = "my_custom_count_prop_col")

    expect_identical(row1, row2)
})

test_that("get_count_prop_rows() works with long = T and custom long_out_col", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_count_prop_rows(ct, long = T, long_out_col = "count_prop_prop"))
    count_prop_long <- get_count_prop_rows(ct, long = T, long_out_col = "count_prop_prop")

    expect_equal(ncol(count_prop_long), 3)
    expect_in("count_prop_prop", names(count_prop_long))
    expect_in("Description", names(count_prop_long))
    expect_in("cohort", names(count_prop_long))
})

test_that("get_count_prop_rows() produces 0% output when na_fill kicks in", {
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

    wide_out <- get_count_prop_rows(ct, long = F)
    expect_true(any(wide_out == "0 (0)"))

    wide_out <- get_count_prop_rows(ct, long = F, round_to = 3)
    expect_true(any(wide_out == "0 (0)"))
})

# add_count_prop_rows() ####
test_that("add_count_prop_rows() adds correct number of rows", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    ct <- add_count_prop_rows(ct)
    expect_gt(nrow(ct), 0)

    row_count_prop_after_first <- nrow(ct)

    ct <- add_count_prop_rows(ct)
    expect_equal(nrow(ct), 2 * row_count_prop_after_first)
})

test_that("add_count_prop_rows() respects round_to", {
    seed <- 4321
    ct1 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_count_prop_rows(ct1, round_to = 0)
    ct2 <- add_count_prop_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_percent_rows() ####
test_that("get_percent_rows() works with proper categorical data", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_percent_rows(ct))
    percent_rows <- get_percent_rows(ct)

    expect_true(nrow(percent_rows) >= 2)
    expect_true(ncol(percent_rows) >= 3)

    expect_true("Description" %in% names(percent_rows))

    cohort_col_names <- setdiff(names(percent_rows), "Description")
})

test_that("get_percent_rows() long = F ignores long_out_col argument", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_percent_rows(ct)
    row2 <- get_percent_rows(ct, long_out_col = "my_custom_percent_col")

    expect_identical(row1, row2)
})

test_that("get_percent_rows() works with long = T and custom long_out_col", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_percent_rows(ct, long = T, long_out_col = "percent_percent"))
    percent_long <- get_percent_rows(ct, long = T, long_out_col = "percent_percent")

    expect_equal(ncol(percent_long), 3)
    expect_in("percent_percent", names(percent_long))
    expect_in("Description", names(percent_long))
    expect_in("cohort", names(percent_long))
})

test_that("get_percent_rows() produces 0% output when na_fill kicks in", {
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

    wide_out <- get_percent_rows(ct, long = F)
    expect_true(any(wide_out == "0%"))
})

# add_percent_rows() ####
test_that("add_percent_rows() adds correct number of rows", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    ct <- add_percent_rows(ct)
    expect_gt(nrow(ct), 0)

    row_percent_after_first <- nrow(ct)

    ct <- add_percent_rows(ct)
    expect_equal(nrow(ct), 2 * row_percent_after_first)
})

test_that("add_percent_rows() respects round_to", {
    seed <- 4321
    ct1 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_percent_rows(ct1, round_to = 0)
    ct2 <- add_percent_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# get_count_percent_rows() ####
test_that("get_count_percent_rows() works with proper categorical data", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_count_percent_rows(ct))
    count_percent_rows <- get_count_percent_rows(ct)

    expect_true(nrow(count_percent_rows) >= 2)
    expect_true(ncol(count_percent_rows) >= 3)

    expect_true("Description" %in% names(count_percent_rows))

    cohort_col_names <- setdiff(names(count_percent_rows), "Description")
})

test_that("get_count_percent_rows() long = F ignores long_out_col argument", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    row1 <- get_count_percent_rows(ct)
    row2 <- get_count_percent_rows(ct, long_out_col = "my_custom_count_percent_col")

    expect_identical(row1, row2)
})

test_that("get_count_percent_rows() works with long = T and custom long_out_col", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_silent(get_count_percent_rows(ct, long = T, long_out_col = "count_percent_percent"))
    count_percent_long <- get_count_percent_rows(ct, long = T, long_out_col = "count_percent_percent")

    expect_equal(ncol(count_percent_long), 3)
    expect_in("count_percent_percent", names(count_percent_long))
    expect_in("Description", names(count_percent_long))
    expect_in("cohort", names(count_percent_long))
})

test_that("get_count_percent_rows() produces 0% output when na_fill kicks in", {
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

    wide_out <- get_count_percent_rows(ct, long = F)
    expect_true(any(wide_out == "0 (0%)"))
})

# add_count_percent_rows() ####
test_that("add_count_percent_rows() adds correct number of rows", {
    ct <- crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort"
    )

    expect_equal(nrow(ct), 0)
    ct <- add_count_percent_rows(ct)
    expect_gt(nrow(ct), 0)

    row_count_percent_after_first <- nrow(ct)

    ct <- add_count_percent_rows(ct)
    expect_equal(nrow(ct), 2 * row_count_percent_after_first)
})

test_that("add_count_percent_rows() respects round_to", {
    seed <- 4321
    ct1 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")
    ct2 <- crosstab(df = cat_test_df(seed = seed), cohort_col_name = "cohort")

    ct1 <- add_count_percent_rows(ct1, round_to = 0)
    ct2 <- add_count_percent_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# add_anova_rows() ####
test_that("add_anova_rows() adds correct number of rows",{
    test_df <- num_test_df(seed = 1)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    expect_silent(add_anova_rows(ct))
    ct <- add_anova_rows(ct)
    expect_equal(nrow(ct), 1)

    test_df <- num_test_df(seed = 1)
    d_cohort <- test_df[["cohort"]] == "D"
    test_df[["variable"]][d_cohort] <- test_df[["variable"]][d_cohort] + 30

    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    expect_silent(add_anova_rows(ct))
    ct <- add_anova_rows(ct)
    expect_equal(nrow(ct), 4)
})

test_that("add_anova_rows() respects round_to",{
    test_df <- num_test_df(seed = 1)
    d_cohort <- test_df[["cohort"]] == "D"
    test_df[["variable"]][d_cohort] <- test_df[["variable"]][d_cohort] + 30

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_anova_rows(ct1, round_to = 0)
    ct2 <- add_anova_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

# add_chisq_rows() ####
test_that("add_chisq_rows() adds correct number of rows",{
    test_df <- cat_test_df(seed = 1)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    expect_silent(add_chisq_rows(ct))
    ct <- add_chisq_rows(ct)
    expect_equal(nrow(ct), 1)

    # Make sure that cohort D will produce a statistically significant difference
    test_df <- cat_test_df(seed = 1, factorize = F)
    d_cohort <- test_df[["cohort"]] == "D"
    d_rows <- test_df[d_cohort, , drop = F]
    rownames(d_rows) <- NULL
    everything_else <- test_df[!d_cohort, , drop = F]
    rownames(everything_else) <- NULL

    d_rows[["variable"]] <- "night"
    d_rows[["variable"]][1:5] <- "morning"
    d_rows[["variable"]][6:10] <- "afternoon"
    d_rows[["variable"]][11:15] <- "evening"

    test_df <- rbind(everything_else, d_rows)

    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    expect_silent(add_chisq_rows(ct))
    ct <- add_chisq_rows(ct)
    expect_equal(nrow(ct), 4)
})

test_that("add_chisq_rows() respects round_to",{

    # Make sure that cohort D will produce a statistically significant difference
    test_df <- cat_test_df(seed = 1, factorize = F)
    d_cohort <- test_df[["cohort"]] == "D"
    d_rows <- test_df[d_cohort, , drop = F]
    rownames(d_rows) <- NULL
    everything_else <- test_df[!d_cohort, , drop = F]
    rownames(everything_else) <- NULL
    d_rows[["variable"]] <- "night"
    d_rows[["variable"]][1:5] <- "morning"
    d_rows[["variable"]][6:10] <- "afternoon"
    d_rows[["variable"]][11:15] <- "evening"
    test_df <- rbind(everything_else, d_rows)

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_chisq_rows(ct1, round_to = 0)
    ct2 <- add_chisq_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

test_that("add_chisq_rows() respects p.adj",{

    # Make sure that cohort D will produce a statistically significant difference
    test_df <- cat_test_df(seed = 1, factorize = F)
    d_cohort <- test_df[["cohort"]] == "D"
    d_rows <- test_df[d_cohort, , drop = F]
    rownames(d_rows) <- NULL
    everything_else <- test_df[!d_cohort, , drop = F]
    rownames(everything_else) <- NULL
    d_rows[["variable"]] <- "night"
    d_rows[["variable"]][1:5] <- "morning"
    d_rows[["variable"]][6:10] <- "afternoon"
    d_rows[["variable"]][11:15] <- "evening"
    test_df <- rbind(everything_else, d_rows)

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_chisq_rows(ct1, p.adj = F)
    ct2 <- add_chisq_rows(ct2, p.adj = T)

    expect_false(identical(ct1, ct2))
})

test_that("add_chisq_rows() respects p-value adjustment method",{

    # Make sure that cohort D will produce a statistically significant difference
    test_df <- cat_test_df(seed = 1, factorize = F)
    d_cohort <- test_df[["cohort"]] == "D"
    d_rows <- test_df[d_cohort, , drop = F]
    rownames(d_rows) <- NULL
    everything_else <- test_df[!d_cohort, , drop = F]
    rownames(everything_else) <- NULL
    d_rows[["variable"]] <- "night"
    d_rows[["variable"]][1:5] <- "morning"
    d_rows[["variable"]][6:10] <- "afternoon"
    d_rows[["variable"]][11:15] <- "evening"
    test_df <- rbind(everything_else, d_rows)

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_chisq_rows(ct1, method = "BH")
    ct2 <- add_chisq_rows(ct2, method = "bonferroni")

    expect_false(identical(ct1, ct2))
})

# add_rao_scott_rows() ####
test_that("add_rao_scott_rows() adds correct number of rows",{
    test_df <- multi_test_df(seed = 1)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    expect_silent(add_rao_scott_rows(ct))
    ct <- add_rao_scott_rows(ct)
    expect_equal(nrow(ct), 1)

    test_df <- multi_test_df(seed = 13)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    expect_silent(add_rao_scott_rows(ct))
    ct <- add_rao_scott_rows(ct)
    expect_equal(nrow(ct), 4)
})

test_that("add_rao_scott_rows() respects round_to",{
    test_df <- multi_test_df(seed = 13)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_rao_scott_rows(ct1, round_to = 0)
    ct2 <- add_rao_scott_rows(ct2, round_to = 3)

    expect_false(identical(ct1, ct2))
})

test_that("add_rao_scott_rows() respects p.adj",{
    test_df <- multi_test_df(seed = 13)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_rao_scott_rows(ct1, p.adj = F)
    ct2 <- add_rao_scott_rows(ct2, p.adj = T)

    expect_false(identical(ct1, ct2))
})

test_that("add_rao_scott_rows() respects p-value adjustment method",{
    test_df <- multi_test_df(seed = 13)
    ct <- crosstab(
        df = test_df,
        cohort_col_name = "cohort"
    )

    ct1 <- crosstab(df = test_df, cohort_col_name = "cohort")
    ct2 <- crosstab(df = test_df, cohort_col_name = "cohort")

    ct1 <- add_rao_scott_rows(ct1, method = "BH")
    ct2 <- add_rao_scott_rows(ct2, method = "bonferroni")

    expect_false(identical(ct1, ct2))
})
