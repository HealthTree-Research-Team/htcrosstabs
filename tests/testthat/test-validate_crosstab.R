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

test_that("validate_crosstab() fails when index attribute is malformed",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    attr(test_ct, "index") <- c(1, 2, 3)
    expect_error(validate_crosstab(test_ct))
    attr(test_ct, "index") <- c(a = "a", b = "b", c = "c")
    expect_error(validate_crosstab(test_ct))
    attr(test_ct, "index") <- NULL
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

# validate_input_index_getter() ####
test_that("validate_input_index_getter() works when given proper data",{
    expect_silent(validate_input_index_getter(crosstab(cat_test_df(), "cohort"), F))
    expect_silent(validate_input_index_getter(crosstab(cat_test_df(), "cohort"), T))
})

test_that("validate_input_index_getter() fails when ct is not a crosstab",{
    expect_error(validate_input_index_getter(crosstab_data(cat_test_df(), "cohort"), F))
    expect_error(validate_input_index_getter(NULL, F))
    expect_error(validate_input_index_getter(1, F))
    expect_error(validate_input_index_getter(c(1, 2, 3), F))
    expect_error(validate_input_index_getter("a", F))
    expect_error(validate_input_index_getter(c("a", "b", "c"), F))
    expect_error(validate_input_index_getter(TRUE, F))
    expect_error(validate_input_index_getter(list(), F))
    expect_error(validate_input_index_getter(data.frame(), F))
})

test_that("validate_input_index_getter() fails when long is not logical",{
    test_ct <- crosstab(cat_test_df(), "cohort")

    expect_error(validate_input_index_getter(test_ct, crosstab_data(cat_test_df(), "cohort")))
    expect_error(validate_input_index_getter(test_ct, NULL))
    expect_error(validate_input_index_getter(test_ct, 1))
    expect_error(validate_input_index_getter(test_ct, c(1, 2, 3)))
    expect_error(validate_input_index_getter(test_ct, "a"))
    expect_error(validate_input_index_getter(test_ct, c("a", "b", "c")))
    expect_error(validate_input_index_getter(test_ct, list()))
    expect_error(validate_input_index_getter(test_ct, data.frame()))
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

test_that("validate_input_data_table_setter() fails when one is grouped and the other isn't",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    new_data <- crosstab_data(cat_test_df(gr = F))
    expect_error(validate_input_data_table_setter(test_ct, new_data))

    test_ct <- crosstab(cat_test_df(gr = F))
    new_data <- crosstab_data(cat_test_df(), "cohort")
    expect_error(validate_input_data_table_setter(test_ct, new_data))
})

test_that("validate_input_data_table_setter() fails when the objects have different cohort columns",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    new_data <- crosstab_data(cat_test_df(group_name = "group"), "group")
    expect_error(validate_input_data_table_setter(test_ct, new_data))
})

test_that("validate_input_data_table_setter() fails when the objects have different factor levels",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    new_data <- crosstab_data(cat_test_df(), "cohort")
    cohort_levels(new_data) <- rev(cohort_levels(new_data))
    expect_error(validate_input_data_table_setter(test_ct, new_data))
})

test_that("validate_input_data_table_setter() fails when the objects have different combined_cohort_names",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    new_data <- crosstab_data(cat_test_df(), "cohort", combined_cohort_name = "All (but different)")
    expect_error(validate_input_data_table_setter(test_ct, new_data))
})

test_that("validate_input_data_table_setter() fails when the objects have different desc_col_name",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    new_data <- crosstab_data(cat_test_df(), "cohort", desc_col_name = "All (but different)")
    expect_error(validate_input_data_table_setter(test_ct, new_data))
})

# validate_input_set_new_data_df() ####
test_that("validate_input_set_new_data_df() works when given proper data",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    expect_silent(validate_input_set_new_data_df(test_ct, data.frame(), NULL))
    expect_silent(validate_input_set_new_data_df(test_ct, data.frame(), c(a = 1, b = 2)))
})

test_that("validate_input_set_new_data_df() fails when ct is not a crosstab",{
    expect_error(validate_input_set_new_data_df(NULL, data.frame(), NULL))
    expect_error(validate_input_set_new_data_df(TRUE, data.frame(), NULL))
    expect_error(validate_input_set_new_data_df(1, data.frame(), NULL))
    expect_error(validate_input_set_new_data_df(c(1, 2, 3), data.frame(), NULL))
    expect_error(validate_input_set_new_data_df("a", data.frame(), NULL))
    expect_error(validate_input_set_new_data_df(c("a", "b", "c"), data.frame(), NULL))
    expect_error(validate_input_set_new_data_df(list(), data.frame(), NULL))
    expect_error(validate_input_set_new_data_df(data.frame(), data.frame(), NULL))
})

test_that("validate_input_set_new_data_df() fails when df is not a data.frame",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    expect_error(validate_input_set_new_data_df(test_ct, NULL, NULL))
    expect_error(validate_input_set_new_data_df(test_ct, TRUE, NULL))
    expect_error(validate_input_set_new_data_df(test_ct, 1, NULL))
    expect_error(validate_input_set_new_data_df(test_ct, c(1, 2, 3), NULL))
    expect_error(validate_input_set_new_data_df(test_ct, "a", NULL))
    expect_error(validate_input_set_new_data_df(test_ct, c("a", "b", "c"), NULL))
    expect_error(validate_input_set_new_data_df(test_ct, list(), NULL))
})

test_that("validate_input_set_new_data_df() fails when df is not a data.frame",{
    test_ct <- crosstab(cat_test_df(), "cohort")
    expect_error(validate_input_set_new_data_df(test_ct, data.frame(), c(1, 2, 3)))
    expect_error(validate_input_set_new_data_df(test_ct, data.frame(), c(a = "a", b = "b", c = "c")))
})

# validate_input_index_setter() ####
test_that("validate_input_index_setter() works when given proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort") |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row()

    expect_silent(validate_input_index_setter(test_ct, c(a = 2, b = 2)))
    expect_silent(validate_input_index_setter(test_ct, c(a = 3, b = 1)))
    expect_silent(validate_input_index_setter(test_ct, c(a = 0, b = 4)))
    expect_silent(validate_input_index_setter(test_ct, c(a = 4, b = 0, c = 0)))
})

test_that("validate_input_index_setter() fails when ct is not a crosstab",{
    expect_error(validate_input_index_setter(NULL, c(a = 2, b = 2)))
    expect_error(validate_input_index_setter(TRUE, c(a = 2, b = 2)))
    expect_error(validate_input_index_setter(1, c(a = 2, b = 2)))
    expect_error(validate_input_index_setter(c(1, 2, 3), c(a = 2, b = 2)))
    expect_error(validate_input_index_setter("a", c(a = 2, b = 2)))
    expect_error(validate_input_index_setter(c("a", "b", "c"), c(a = 2, b = 2)))
    expect_error(validate_input_index_setter(list(), c(a = 2, b = 2)))
    expect_error(validate_input_index_setter(data.frame(), c(a = 2, b = 2)))
})

test_that("validate_input_index_setter() fails when value is not a named numeric vector",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_error(validate_input_index_setter(test_ct, c(1, 2, 3)))
    expect_error(validate_input_index_setter(test_ct, c("a", "b", "c")))
    expect_error(validate_input_index_setter(test_ct, c(a = "a", b = "b", c = "c")))
})

test_that("validate_input_index_setter() fails when the new index has NA values",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort") |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row()

    expect_error(validate_input_index_setter(test_ct, c(a = 2, b = NA)))
    expect_error(validate_input_index_setter(test_ct, c(a = NA, b = 2)))
})

test_that("validate_input_index_setter() fails when the new index doesn't have the same sum of rows as the old index",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort") |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row()

    expect_error(validate_input_index_setter(test_ct, c(a = 2, b = 1)))
    expect_error(validate_input_index_setter(test_ct, c(a = 0, b = 2)))
    expect_error(validate_input_index_setter(test_ct, c(a = 5, b = 1)))
    expect_error(validate_input_index_setter(test_ct, c(a = 5, b = 5, c = 5)))
})

# validate_input_stack_crosstabs() ####
test_that("validate_input_stack_crosstabs() works when given proper data",{
    expect_silent(validate_input_stack_crosstabs(list(
        crosstab(cat_test_df(col_name = "cat"), "cohort"),
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))
})

test_that("validate_input_stack_crosstabs() works when given proper data",{
    expect_error(validate_input_stack_crosstabs(list(
        NULL,
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        NA,
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        TRUE,
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        1,
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        c(1, 2, 3),
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        "a",
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        c("a", "b", "c"),
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        list(),
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        data.frame(),
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))

    expect_error(validate_input_stack_crosstabs(list(
        crosstab_data(cat_test_df(col_name = "cat"), "cohort"),
        crosstab(num_test_df(col_name = "num"), "cohort"),
        crosstab(lik_test_df(col_name = "lik"), "cohort"),
        crosstab(multi_test_df(col_name = "mul"), "cohort")
    )))
})
