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

test_that("new_crosstab() errors when likert_map isn't a named numeric vector",{
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]]) |> as.character()
    expect_error(new_crosstab(df, "cohort", map))

    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    names(map) <- NULL
    expect_error(new_crosstab(df, "cohort", map))
})

test_that("new_crosstab() errors when likert_map doesn't map all values",{
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    map <- map[2:length(map)]
    expect_error(new_crosstab(df, "cohort", map))
})

test_that("new_crosstab() errors when default_cohort isn't a character",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_error(new_crosstab(df, "cohort", default_cohort = 1))
    expect_error(new_crosstab(df, "cohort", default_cohort = NA))
    expect_error(new_crosstab(df, "cohort", default_cohort = TRUE))
})

# VALIDATORS ####
test_that("validate_crosstab() works when given correct data",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct <- new_crosstab(df, "cohort")
    expect_true(validate_crosstab(ct))
})

test_that("validate_crosstab() errors when ct isn't a crosstab",{
    expect_error(validate_crosstab(NULL))
    expect_error(validate_crosstab(NA))
    expect_error(validate_crosstab(1))
    expect_error(validate_crosstab("a"))
    expect_error(validate_crosstab(c("a", "b")))
    expect_error(validate_crosstab(TRUE))
    expect_error(validate_crosstab(list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(validate_crosstab(data.frame()))
    ct_data <- get_categorical_test_df() |>
        factorize_columns() |>
        crosstab_data()
    expect_error(validate_crosstab(ct_data))
})

test_that("validate_crosstab() errors when ct has no \"data\" attribute",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct <- new_crosstab(df, "cohort")
    attr(ct, "data") <- NULL
    expect_error(validate_crosstab(ct))
})

# GETTERS ####
test_that("data() returns the proper table",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct_data <- crosstab_data(df, "cohort")
    ct <- crosstab(df, "cohort")
    expect_identical(get_data(ct), ct_data)
})

test_that("data() errors when ct isn't a crosstab",{
    expect_error(get_data(NULL))
    expect_error(get_data(NA))
    expect_error(get_data(TRUE))
    expect_error(get_data(1))
    expect_error(get_data(c(1, 2)))
    expect_error(get_data("a"))
    expect_error(get_data(c("a", "b")))
    expect_error(get_data(list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(get_data(data.frame()))

    ct_data <- get_categorical_test_df() |>
        factorize_columns() |>
        crosstab_data()
    expect_error(get_data(ct_data))
})

# SETTERS ####
test_that("set_data<- works when given correct data",{
    ct <- get_categorical_test_df(gr = T) |>
        factorize_columns() |>
        crosstab("cohort")
    new_ct_data <- get_numeric_test_df(gr = T) |>
        factorize_columns() |>
        crosstab_data("cohort")

    set_data(ct) <- new_ct_data
    expect_identical(get_data(ct), new_ct_data)
})

test_that("set_data<- errors when ct isn't a crosstab",{
    ct_data <- get_categorical_test_df(gr = T) |>
        factorize_columns() |>
        crosstab_data("cohort")
    expect_error(`set_data<-`(NULL, ct_data))
    expect_error(`set_data<-`(NA, ct_data))
    expect_error(`set_data<-`(TRUE, ct_data))
    expect_error(`set_data<-`(1, ct_data))
    expect_error(`set_data<-`(c(1, 2), ct_data))
    expect_error(`set_data<-`("a", ct_data))
    expect_error(`set_data<-`(c("a", "b"), ct_data))
    expect_error(`set_data<-`(list(a = c(1, 2, 3), b = c(4, 5, 6)), ct_data))
    expect_error(`set_data<-`(data.frame(), ct_data))
    expect_error(`set_data<-`(ct_data, ct_data))
})

test_that("set_data<- errors when value isn't a crosstab_data",{
    ct <- get_categorical_test_df(gr = T) |>
        factorize_columns() |>
        crosstab("cohort")
    expect_error(`set_data<-`(ct, NULL))
    expect_error(`set_data<-`(ct, NA))
    expect_error(`set_data<-`(ct, TRUE))
    expect_error(`set_data<-`(ct, 1))
    expect_error(`set_data<-`(ct, c(1, 2)))
    expect_error(`set_data<-`(ct, "a"))
    expect_error(`set_data<-`(ct, c("a", "b")))
    expect_error(`set_data<-`(ct, list(a = c(1, 2, 3), b = c(4, 5, 6))))
    expect_error(`set_data<-`(ct, data.frame()))
    expect_error(`set_data<-`(ct, ct))
})
