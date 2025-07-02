test_that("total() returns the proper number of rows for ungrouped data",{
    ct <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab()

    total_table <- get_total(ct)
    expect_equal(nrow(total_table), 1)
    expect_equal(ncol(total_table), 2)
    expect_true(all(total_table[["cohort"]] %in% c("All")))
    expect_true(all(total_table[["total"]] == 200))

    ct_data <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab_data()

    total_table <- get_total(ct_data)
    expect_equal(nrow(total_table), 1)
    expect_equal(ncol(total_table), 2)
    expect_true(all(total_table[["cohort"]] %in% c("All")))
    expect_true(all(total_table[["total"]] == 200))
})

test_that("total() returns the proper number of rows for grouped data",{
    ct <- get_categorical_test_df(gr = T, nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab("cohort")

    total_table <- get_total(ct)
    expect_equal(nrow(total_table), 6) # including NA in cohort column
    expect_equal(ncol(total_table), 2)
})

test_that("total() errors when ct_data isn't of type crosstab",{
    expect_error(get_total(NULL))
    expect_error(get_total(NA))
    expect_error(get_total(TRUE))
    expect_error(get_total(1))
    expect_error(get_total(c(1, 2)))
    expect_error(total("a"))
    expect_error(get_total(c("a", "b")))
    expect_error(get_total(list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(get_total(data.frame()))
})

test_that("complete() returns the proper number of rows for ungrouped data",{
    ct <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab_data()

    total_table <- get_total(ct)
    expect_equal(nrow(total_table), 1)
    expect_equal(ncol(total_table), 2)
})

test_that("complete() returns the proper number of rows for grouped data",{
    ct <- get_categorical_test_df(gr = T, nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab("cohort")

    complete_table <- get_complete(ct)
    expect_equal(nrow(complete_table), 6) # including NA in cohort column
    expect_equal(ncol(complete_table), 2)

    ct_data <- get_categorical_test_df(gr = T, nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab_data("cohort")

    complete_table <- get_complete(ct_data)
    expect_equal(nrow(complete_table), 6) # including NA in cohort column
    expect_equal(ncol(complete_table), 2)
})

test_that("complete() errors when ct_data isn't of type crosstab",{
    expect_error(get_complete(NULL))
    expect_error(get_complete(NA))
    expect_error(get_complete(TRUE))
    expect_error(get_complete(1))
    expect_error(get_complete(c(1, 2)))
    expect_error(complete("a"))
    expect_error(get_complete(c("a", "b")))
    expect_error(get_complete(list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(get_complete(data.frame()))
})

test_that("add_total_row() adds the total row",{
    ct <- get_categorical_test_df(gr = T, nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab("cohort")

    expect_equal(nrow(ct), 0)
    ct <- add_total_row(ct)
    expect_equal(nrow(ct), 1)
    ct <- add_total_row(ct)
    expect_equal(nrow(ct), 2)

    ct <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab()

    expect_equal(nrow(ct), 0)
    ct <- add_total_row(ct)
    expect_equal(nrow(ct), 1)
    ct <- add_total_row(ct)
    expect_equal(nrow(ct), 2)
})

test_that("mean() returns the proper table",{
    ct_data <- get_numeric_test_df(gr = T, nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab_data("cohort")

    mean_table <- get_mean(ct_data)
    expect_equal(nrow(mean_table), 6) # including NA in cohort column
    expect_equal(ncol(mean_table), 2)

    ct <- get_numeric_test_df(gr = T, nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab("cohort")

    mean_table <- get_mean(ct_data)
    expect_equal(nrow(mean_table), 6) # including NA in cohort column
    expect_equal(ncol(mean_table), 2)


})
