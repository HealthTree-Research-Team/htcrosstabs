test_that("total() returns the proper number of rows",{
    ct_data <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab_data()
    expect_equal(total(ct_data), 200)
})

test_that("total() errors when ct_data isn't of type crosstab_data",{
    expect_error(total(NULL))
    expect_error(total(NA))
    expect_error(total(TRUE))
    expect_error(total(1))
    expect_error(total(c(1, 2)))
    expect_error(total("a"))
    expect_error(total(c("a", "b")))
    expect_error(total(list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(total(data.frame()))

    ct <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab()
    expect_error(total(ct))
})

test_that("complete() errors when ct_data isn't of type crosstab_data",{
    expect_error(complete(NULL))
    expect_error(complete(NA))
    expect_error(complete(TRUE))
    expect_error(complete(1))
    expect_error(complete(c(1, 2)))
    expect_error(complete("a"))
    expect_error(complete(c("a", "b")))
    expect_error(complete(list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(complete(data.frame()))

    ct <- get_categorical_test_df(nrows = 200, w_na = T) |>
        factorize_columns() |>
        crosstab()
    expect_error(complete(ct))
})
