# add_default_table()
test_that("add_default_table() functions when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)

    expect_true(is.crosstab.categorical(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 6)
    expect_equal(ncol(test_ct), 2)
})

test_that("add_default_table() functions when given grouped categorical data",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")

    expect_true(is.crosstab.categorical(test_ct))
    expect_true(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 6)
    expect_equal(ncol(test_ct), 7)
})

test_that("add_default_table() functions when given ungrouped numeric data",{
    test_df <- num_test_df(gr = F)
    test_ct <- crosstab(test_df)

    expect_true(is.crosstab.numeric(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

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

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 3)
    expect_equal(ncol(test_ct), 7)
})

test_that("add_default_table() functions when given ungrouped likert data",{
    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, var_map = test_map)

    expect_true(is.crosstab.likert(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 8)
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

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 8)
    expect_equal(ncol(test_ct), 7)
})

test_that("add_default_table() functions when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = F)
    test_ct <- crosstab(test_df)

    expect_true(is.crosstab.multi(test_ct))
    expect_false(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 8)
    expect_equal(ncol(test_ct), 2)
})

test_that("add_default_table() functions when given grouped multianswer data",{
    test_df <- multi_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")

    expect_true(is.crosstab.multi(test_ct))
    expect_true(is.crosstab.grouped(test_ct))
    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    expect_silent(add_default_table(test_ct))

    expect_equal(nrow(test_ct), 0)
    expect_equal(ncol(test_ct), 0)

    test_ct <- add_default_table(test_ct)

    expect_equal(nrow(test_ct), 8)
    expect_equal(ncol(test_ct), 7)
})
