# validate_input_new_crosstab() ####
test_that("validate_input_new_crosstab() works when given proper data",{
    expect_silent(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    expect_silent(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = test_map,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))
})

test_that("validate_input_new_crosstab() fails when df is not a data frame",{
    expect_error(validate_input_new_crosstab(
        df = NULL,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = TRUE,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = 1,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = c(1, 2, 3),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = "a",
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = c("a", "b", "c"),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = list(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = structure(data.frame, class = "test"),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))
})

test_that("validate_input_new_crosstab() fails when cohort_col_name is not a character",{
    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = 1,
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = c(1, 2, 3),
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = TRUE,
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = list(),
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = data.frame(),
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))
})

test_that("validate_input_new_crosstab() fails when var_map is not a named numeric vector",{
    test_df <- lik_test_df()

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = 1,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = TRUE,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = "a",
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = c("a", "b", "c"),
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = list(),
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = data.frame(),
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    test_map <- default_var_map(test_df[["variable"]])
    new_map <- test_map
    names(new_map) <- NULL

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = new_map,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    new_map <- as.character(test_map)
    names(new_map) <- names(test_map)

    expect_error(validate_input_new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = new_map,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))
})

test_that("validate_input_new_crosstab() fails when combined_cohort_name is not a character",{
    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = NULL,
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = 1,
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = TRUE,
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = list(),
        desc_col_name = "Description"
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = data.frame(),
        desc_col_name = "Description"
    ))
})

test_that("validate_input_new_crosstab() fails when combined_cohort_name is not a character",{
    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = NULL
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = 1
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = c(1, 2, 3)
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = list()
    ))

    expect_error(validate_input_new_crosstab(
        df = cat_test_df(),
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "All",
        desc_col_name = data.frame()
    ))
})

# validate_crosstab() ####
test_that("validate_crosstab() works when given proper data",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(validate_crosstab(test_ct))

    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(validate_crosstab(test_ct))

    test_df <- num_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(validate_crosstab(test_ct))

    test_df <- num_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(validate_crosstab(test_ct))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, var_map = test_map)
    expect_silent(validate_crosstab(test_ct))

    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", var_map = test_map)
    expect_silent(validate_crosstab(test_ct))

    test_df <- multi_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(validate_crosstab(test_ct))

    test_df <- multi_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(validate_crosstab(test_ct))
})

test_that("validate_crosstab() fails when missing data attribute",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    attr(test_ct, "data") <- NULL
    expect_error(validate_crosstab(test_ct))
})

test_that("validate_crosstab() fails when data attribute is malformed",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "var_col_name") <- NULL
    attr(test_ct, "data") <- ct_data
    expect_error(validate_crosstab(test_ct))
})

# validate_input_data_table_getter() ####
test_that("validate_input_data_table_getter() works when given proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(validate_input_data_table_getter(test_ct, F))
    expect_silent(validate_input_data_table_getter(test_ct, T))
})

test_that("validate_input_data_table_getter() fails when ct is not a crosstab",{
    expect_error(validate_input_data_table_getter(crosstab_data(cat_test_df(), "cohort"), T))
    expect_error(validate_input_data_table_getter(NULL, T))
    expect_error(validate_input_data_table_getter(1, T))
    expect_error(validate_input_data_table_getter(c(1, 2, 3), T))
    expect_error(validate_input_data_table_getter("a", T))
    expect_error(validate_input_data_table_getter(c("a", "b", "c"), T))
    expect_error(validate_input_data_table_getter(TRUE, T))
    expect_error(validate_input_data_table_getter(list(), T))
    expect_error(validate_input_data_table_getter(data.frame(), T))
})

test_that("validate_input_data_table_getter() fails when ct is missing data attribute",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    attr(test_ct, "data") <- NULL
    expect_error(validate_input_data_table_getter(test_ct))
})

test_that("validate_input_data_table_getter() fails when raw is not logical",{
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), NULL))
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), 1))
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), c(1, 2, 3)))
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), "a"))
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), c("a", "b", "c")))
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), list()))
    expect_error(validate_input_data_table_getter(crosstab(cat_test_df(), "cohort"), data.frame()))
})

# validate_input_data_table_setter() ####
test_that("validate_input_data_table_setter() works when given proper data",{
    test_df1 <- cat_test_df()
    test_df2 <- num_test_df()
    test_ct <- crosstab(test_df1, "cohort")
    ct_data <- crosstab_data(test_df2, "cohort")
    expect_silent(validate_input_data_table_setter(test_ct, ct_data))
})

test_that("validate_input_data_table_setter() fails when value is not a crosstab_data object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_error(validate_input_data_table_setter(test_ct, NULL))
    expect_error(validate_input_data_table_setter(test_ct, TRUE))
    expect_error(validate_input_data_table_setter(test_ct, 1))
    expect_error(validate_input_data_table_setter(test_ct, c(1, 2, 3)))
    expect_error(validate_input_data_table_setter(test_ct, "a"))
    expect_error(validate_input_data_table_setter(test_ct, c("a", "b", "c")))
    expect_error(validate_input_data_table_setter(test_ct, list()))
    expect_error(validate_input_data_table_setter(test_ct, data.frame()))
})

test_that("validate_input_data_table_setter() fails when value is not a crosstab_data object",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_input_data_table_setter(NULL, ct_data))
    expect_error(validate_input_data_table_setter(TRUE))
    expect_error(validate_input_data_table_setter(1, ct_data))
    expect_error(validate_input_data_table_setter(c(1, 2, 3), ct_data))
    expect_error(validate_input_data_table_setter("a", ct_data))
    expect_error(validate_input_data_table_setter(c("a", "b", "c"), ct_data))
    expect_error(validate_input_data_table_setter(list(), ct_data))
    expect_error(validate_input_data_table_setter(data.frame(), ct_data))
})
