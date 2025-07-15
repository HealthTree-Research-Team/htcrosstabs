# new_crosstab() ####
test_that("new_crosstab() works when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = F)

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.categorical(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given grouped categorical data",{
    test_df <- cat_test_df(gr = T)

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.categorical(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given ungrouped numeric data",{
    test_df <- num_test_df(gr = F)

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.numeric(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given grouped numeric data",{
    test_df <- num_test_df(gr = T)

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.numeric(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given ungrouped likert data",{
    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = test_map,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = test_map,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.likert(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given grouped likert data",{
    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = test_map,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = test_map,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.likert(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = F)

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = NULL,
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.multi(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("new_crosstab() works when given grouped multianswer data",{
    test_df <- multi_test_df(gr = T)

    expect_silent(new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    ))

    ct <- new_crosstab(
        df = test_df,
        cohort_col_name = "cohort",
        var_map = NULL,
        combined_cohort_name = "all",
        desc_col_name = "description"
    )

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.multi(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

# crosstab() ####
test_that("crosstab() works when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = F)
    expect_silent(crosstab(test_df))
    ct <- crosstab(test_df)

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.categorical(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = T)
    expect_silent(crosstab(test_df, "cohort"))
    ct <- crosstab(test_df, "cohort")

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.categorical(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped numeric data",{
    test_df <- num_test_df(gr = F)
    expect_silent(crosstab(test_df))
    ct <- crosstab(test_df)

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.numeric(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped numeric data",{
    test_df <- num_test_df(gr = T)
    expect_silent(crosstab(test_df, "cohort"))
    ct <- crosstab(test_df, "cohort")

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.numeric(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped likert data",{
    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    expect_silent(crosstab(test_df, var_map = test_map))
    ct <- crosstab(test_df, var_map = test_map)

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.likert(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped likert data",{
    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])
    expect_silent(crosstab(test_df, "cohort", var_map = test_map))
    ct <- crosstab(test_df, "cohort", var_map = test_map)

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.likert(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = F)
    expect_silent(crosstab(test_df))
    ct <- crosstab(test_df)

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.multi(ct))
    expect_false(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

test_that("crosstab() works when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = T)
    expect_silent(crosstab(test_df, "cohort"))
    ct <- crosstab(test_df, "cohort")

    expect_s3_class(ct, CT_CLASS)
    expect_s3_class(ct, "data.frame")
    expect_true(is.crosstab(ct, strict = T))
    expect_true(is.crosstab.multi(ct))
    expect_true(is.crosstab.grouped(ct))

    expect_equal(nrow(ct), 0)
    expect_equal(ncol(ct), 0)
    expect_false(is.null(attr(ct, "data")))

    ct_data <- data_table(ct, raw = T)
    expect_equal(ct_data[["variable"]], test_df[["variable"]])
})

# GETTERS ####
test_that("data_table() works when given ungrouped data",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(data_table(test_ct))
    ct_data <- data_table(test_ct)
    expect_equal(test_df[["variable"]], ct_data[["variable"]])
})

test_that("data_table() works the same on ungrouped data whether raw = T or F",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(data_table(test_ct, raw = F))
    expect_silent(data_table(test_ct, raw = T))
    ct_data_raw <- data_table(test_ct, raw = T)
    ct_data_not_raw <- data_table(test_ct, raw = F)
    expect_equal(ct_data_raw[["variable"]], ct_data_not_raw[["variable"]])
})

test_that("data_table() works when given grouped data",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(data_table(test_ct, raw = F))
    ct_data <- data_table(test_ct, raw = F)
    expect_equal(2 * nrow(test_df), nrow(ct_data))
    expect_true(all(test_df[["variable"]] == ct_data[["variable"]], na.rm = T))
})

test_that("data_table() works to remove grouping data when raw = T",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(data_table(test_ct))
    ct_data <- data_table(test_ct, raw = T)
    expect_equal(nrow(test_df), nrow(ct_data))
    expect_true(all(test_df[["variable"]] == ct_data[["variable"]], na.rm = T))
})

test_that("desc_name() passes the call onto the data object",{
    test_ct <- crosstab(cat_test_df(), "cohort", desc_col_name = "test_desc_col")
    expect_silent(desc_name(test_ct))
    expect_equal(desc_name(test_ct), "test_desc_col")
})

test_that("var_name() passes the call onto the data object",{
    test_ct <- crosstab(cat_test_df(col_name = "test_col"), "cohort")
    expect_silent(var_name(test_ct))
    expect_equal(var_name(test_ct), "test_col")
})

test_that("var() passes the call onto the data object",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(var(test_ct))
    expect_equal(var(test_ct), test_df[["variable"]])

    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(var(test_ct, raw = T))
    expect_equal(var(test_ct, raw = T), test_df[["variable"]])
})

test_that("var_levels() passes the call onto the data object",{
    test_df <- cat_test_df(gr = T)
    test_var_levels <- levels(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(var_levels(test_ct))
    expect_equal(var_levels(test_ct), test_var_levels)
})

test_that("cohort_name() passes the call onto the data object",{
    test_df <- cat_test_df(gr = T, group_name = "test_group")
    test_ct <- crosstab(test_df, "test_group")
    expect_silent(cohort_name(test_ct))
    expect_equal(cohort_name(test_ct), "test_group")
})

test_that("cohort() passes the call onto the data object",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(cohort(test_ct))
    expect_false(is.null(cohort(test_ct)))

    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(cohort(test_ct))
    expect_true("All" %in% cohort(test_ct))
    expect_equal(2 * nrow(test_df), length(cohort(test_ct)))
    expect_silent(cohort(test_ct, raw = T))
    expect_false("All" %in% cohort(test_ct, raw = T))
    expect_equal(nrow(test_df), length(cohort(test_ct, raw = T)))
})

test_that("cohort_levels() passes the call onto the data object",{
    test_df <- cat_test_df(gr = T)
    test_cohort_levels <- levels(test_df[["cohort"]])
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(cohort_levels(test_ct))
    expect_equal(cohort_levels(test_ct), c("All", test_cohort_levels))

    expect_silent(cohort_levels(test_ct, raw = T))
    expect_equal(cohort_levels(test_ct, raw = T), test_cohort_levels)
})

test_that("var_map() passes the call onto the data object",{
    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", var_map = test_map)
    expect_silent(var_map(test_ct))
    expect_equal(var_map(test_ct), test_map)
})

test_that("var_mapped() passes the call onto the data object",{
    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])
    mapped_df <- test_df
    mapped_df[["variable"]] <- test_map[as.character(mapped_df[["variable"]])]
    test_ct <- crosstab(test_df, "cohort", var_map = test_map)

    expect_silent(var_mapped(test_ct))
    var_mapped_vals <- var_mapped(test_ct, raw = T)
    names(var_mapped_vals) <- NULL
    expect_equal(var_mapped_vals, mapped_df[["variable"]])
})

test_that("combined_cohort_name() passes the call onto the data object",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort", combined_cohort_name = "test_comb")
    expect_silent(combined_cohort_name(test_ct))
    expect_equal(combined_cohort_name(test_ct), "test_comb")
})

test_that("get_raw_data() passes the call onto the data object",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    expect_silent(get_raw_data(test_ct))
    expect_equal(nrow(test_df), nrow(get_raw_data(test_ct)))

    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    expect_silent(get_raw_data(test_ct))
    expect_equal(nrow(test_df), nrow(get_raw_data(test_ct)))
})

test_that("data_table<-() passes the call onto the data object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    new_df <- num_test_df()
    new_data <- crosstab_data(new_df, "cohort")

    orig_data <- data_table(test_ct)
    expect_silent(`data_table<-`(test_ct, new_data))
    data_table(test_ct) <- new_data
    expect_equal(data_table(test_ct), new_data)
})

test_that("var_name<-() passes the call onto the data object",{
    test_df <- cat_test_df(col_name = "orig_var")
    test_ct <- crosstab(test_df, "cohort")

    expect_equal(var_name(test_ct), "orig_var")
    expect_silent(`var_name<-`(test_ct, "new_var"))
    var_name(test_ct) <- "new_var"
    expect_equal(var_name(test_ct), "new_var")
})

test_that("var<-() passes the call onto the data object",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)
    new_df <- cat_test_df(gr = F)

    expect_false(all(test_df[["variable"]] == new_df[["variable"]], na.rm = T))
    expect_equal(test_df[["variable"]], var(test_ct))
    var(test_ct) <- new_df[["variable"]]
    expect_equal(new_df[["variable"]], var(test_ct))
})

test_that("var_levels<-() passes the call onto the data object",{
    test_df <- cat_test_df(gr = F)
    test_ct <- crosstab(test_df)

    orig_levels <- var_levels(test_ct)
    new_levels <- rev(orig_levels)

    expect_false(all(orig_levels == new_levels, na.rm = T))
    expect_equal(var_levels(test_ct), orig_levels)
    var_levels(test_ct) <- new_levels
    expect_equal(var_levels(test_ct), new_levels)
})

test_that("cohort_name<-() passes the call onto the data object",{
    test_df <- cat_test_df(group_name = "orig_cohort")
    test_ct <- crosstab(test_df, "orig_cohort")

    expect_equal(cohort_name(test_ct), "orig_cohort")
    expect_silent(`cohort_name<-`(test_ct, "new_cohort"))
    var_name(test_ct) <- "new_cohort"
    expect_equal(var_name(test_ct), "new_cohort")
})

test_that("cohort<-() passes the call onto the data object",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")
    new_df <- cat_test_df(gr = T, group_type = "n")

    old_cohort <- test_df[["cohort"]]
    new_cohort <- new_df[["cohort"]]

    expect_equal(cohort(test_ct, raw = T), old_cohort)
    expect_silent(`cohort<-`(test_ct, c(new_cohort, factor(rep(combined_cohort_name(test_ct), length(new_cohort))))))
    cohort(test_ct) <- c(new_cohort, factor(rep(combined_cohort_name(test_ct), length(new_cohort))))
    expect_equal(cohort(test_ct, raw = T), new_cohort)
})

test_that("cohort_levels<-() passes the call onto the data object",{
    test_df <- cat_test_df(gr = T)
    test_ct <- crosstab(test_df, "cohort")

    old_levels <- cohort_levels(test_ct)
    new_levels <- rev(old_levels)

    expect_equal(cohort_levels(test_ct), old_levels)
    expect_silent(`cohort_levels<-`(test_ct, new_levels))
    cohort_levels(test_ct) <- new_levels
    expect_equal(cohort_levels(test_ct), new_levels)
})

test_that("var_map<-() passes the call onto the data object",{
    test_df <- lik_test_df(gr = T)
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", var_map = test_map)
    new_map <- test_map
    names(new_map) <- rev(names(test_map))

    expect_equal(var_map(test_ct), test_map)
    expect_silent(`var_map<-`(test_ct, new_map))
    var_map(test_ct) <- new_map
    expect_equal(var_map(test_ct), new_map)
})
