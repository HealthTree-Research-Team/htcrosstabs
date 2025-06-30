# CONSTRUCTORS ####
test_that("new_crosstab() works when given proper data",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_no_error(new_crosstab(df, "cohort"))

    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    expect_no_error(new_crosstab(df, "cohort"))

    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    expect_no_error(new_crosstab(df, "cohort"))

    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    expect_no_error(new_crosstab(df, "cohort"))
})

test_that("new_crosstab() errors when df is not a data.frame()",{
    expect_error(new_crosstab(1))
    expect_error(new_crosstab(NULL))
    expect_error(new_crosstab(c("a", "b")))
    expect_error(new_crosstab(list(a = c(1, 2, 3), b = c(4, 5, 6))))
})

test_that("new_crosstab() errors when cohort_col_name isn't a character",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_error(new_crosstab(df, 1))
    expect_error(new_crosstab(df, NULL))
    expect_error(new_crosstab(df, c("a", "b")))
    expect_error(new_crosstab(df, list(a = c(1, 2, 3), b = c(4, 5, 6))))
})

test_that("new_crosstab() errors when cohort_col_name isn't in df",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_error(new_crosstab(df, "test"))
})

test_that("new_crosstab()")
