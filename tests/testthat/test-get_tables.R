# add_categorical_table() ####
test_that("add_categorical_table() works when given proper data",{
    test_ct <- cat_test_df() |> crosstab("cohort")
    expect_silent(add_categorical_table(test_ct, chisq = F))
    result <- add_categorical_table(test_ct, chisq = F)

    expect_equal(nrow(result), 5)
    expect_equal(ncol(result), 6)

    test_ct <- cat_test_df() |> crosstab("cohort")
    expect_silent(add_categorical_table(test_ct, chisq = F))
    result <- add_categorical_table(test_ct, chisq = F, keep_na_vars = T)

    expect_equal(nrow(result), 6)
    expect_equal(ncol(result), 6)
})

test_that("add_categorical_table() works when given other types of data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_categorical_table(test_ct, chisq = F))
    result <- suppressWarnings(add_categorical_table(test_ct, chisq = F))

    expect_gt(nrow(result), 3)
    expect_equal(ncol(result), 6)

    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_categorical_table(test_ct, chisq = F))
    result <- add_categorical_table(test_ct, chisq = F)

    expect_equal(nrow(result), 7)
    expect_equal(ncol(result), 6)

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", test_map)
    expect_silent(add_categorical_table(test_ct, chisq = F))
    result <- add_categorical_table(test_ct, chisq = F)

    expect_equal(nrow(result), 6)
    expect_equal(ncol(result), 6)
})

# add_numeric_table() ####
test_that("add_numeric_table() works when given proper data",{
    test_ct <- num_test_df() |> crosstab("cohort")
    expect_silent(add_numeric_table(test_ct, anova = F))
    result <- add_numeric_table(test_ct, anova = F)

    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 6)
})

test_that("add_numeric_table() works when given other types of data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_numeric_table(test_ct, anova = F))
    result <- suppressWarnings(add_numeric_table(test_ct, anova = F))

    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 6)

    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_numeric_table(test_ct, anova = F))
    result <- suppressWarnings(add_numeric_table(test_ct, anova = F))

    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 6)

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", test_map)
    expect_silent(add_numeric_table(test_ct, anova = F))
    result <- add_numeric_table(test_ct, anova = F)

    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 6)
})

# add_likert_table() ####
test_that("add_likert_table() works when given proper data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", test_map)
    expect_silent(add_likert_table(test_ct, anova = F, chisq = F))
    result <- add_likert_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(result), 7)
    expect_equal(ncol(result), 6)
})

test_that("add_likert_table() works when given other types of data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_likert_table(test_ct, anova = F, chisq = F))
    result <- suppressWarnings(add_likert_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(result), 6)
    expect_equal(ncol(result), 6)

    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_likert_table(test_ct, anova = F, chisq = F))
    result <- suppressWarnings(add_likert_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(result), 8)
    expect_equal(ncol(result), 6)

    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(add_likert_table(test_ct, anova = F, chisq = F))
    result <- suppressWarnings(add_likert_table(test_ct, anova = F, chisq = F))

    expect_gt(nrow(result), 7)
    expect_equal(ncol(result), 6)
})

# add_default_table() ####
test_that("add_default_table() functions when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)

    expect_true(is.crosstab.categorical(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 5)
    expect_equal(ncol(test_ct), 2)
})

test_that("add_default_table() functions when given grouped categorical data",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")

    expect_true(is.crosstab.categorical(test_ct))
    expect_true(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 5)
    expect_equal(ncol(test_ct), 6)
})

test_that("add_default_table() functions when given ungrouped numeric data",{
    test_df <- num_test_df(gr = F)
    test_ct <- crosstab(test_df)

    expect_true(is.crosstab.numeric(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 3)
    expect_equal(ncol(test_ct), 2)
})

test_that("add_default_table() functions when given grouped numeric data",{
    test_df <- num_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")

    expect_true(is.crosstab.numeric(test_ct))
    expect_true(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 3)
    expect_equal(ncol(test_ct), 6)
})

test_that("add_default_table() functions when given ungrouped likert data",{
    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, var_map = test_map)

    expect_true(is.crosstab.likert(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 7)
    expect_equal(ncol(test_ct), 2)
})

test_that("add_default_table() functions when given grouped likert data",{
    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", var_map = test_map)

    expect_true(is.crosstab.likert(test_ct))
    expect_true(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 7)
    expect_equal(ncol(test_ct), 6)
})

test_that("add_default_table() functions when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = F)
    test_ct <- crosstab(test_df)

    expect_true(is.crosstab.multi(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 7)
    expect_equal(ncol(test_ct), 2)
})

test_that("add_default_table() functions when given grouped multianswer data",{
    test_df <- multi_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")

    expect_true(is.crosstab.multi(test_ct))
    expect_true(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct, anova = F, chisq = F))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct, anova = F, chisq = F)

    expect_equal(nrow(test_ct), 7)
    expect_equal(ncol(test_ct), 6)
})

# crosstab_stacked() ####
test_that("crosstab_stacked() works when given proper data",{
    test_df <- cat_test_df(col_name = "cat")
    test_df[["num"]] <- num_test_df(col_name = "var2")[["var2"]]
    test_df[["lik"]] <- lik_test_df(col_name = "var3")[["var3"]]
    test_df[["mul"]] <- multi_test_df(col_name = "var4")[["var4"]]
    test_df <- test_df[, c("cat", "num", "lik", "mul", "cohort"), drop = F]

    test_map <- default_var_map(test_df[["lik"]])

    test_ct <- crosstab_stacked(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = test_map,
        anova = F,
        chisq = F
    )

    expect_equal(nrow(test_ct), 22)
    expect_equal(ncol(test_ct), 6)

    expect_equal(sum(index(test_ct)), 22)
    expect_equal(length(unique(table_id(test_ct))), 3)
})

