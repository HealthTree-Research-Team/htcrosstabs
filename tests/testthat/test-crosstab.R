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

test_that("index() returns the proper index",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort") |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row()

    expect_silent(index(test_ct))
    short_index <- index(test_ct)
    expect_equal(short_index, c("variable" = 4))

    expect_silent(index(test_ct, long = T))
    long_index <- index(test_ct, long = T)

    expect_equal(length(long_index), sum(short_index))
    expect_true(all(long_index %in% names(short_index)))
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

# SETTERS ####
test_that("data_table<-() sets the data object to the new object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    new_df <- num_test_df()
    new_data <- crosstab_data(new_df, "cohort")

    orig_index <- attr(test_ct, "index")

    orig_data <- data_table(test_ct)
    expect_silent(`data_table<-`(test_ct, new_data))
    data_table(test_ct) <- new_data
    expect_equal(data_table(test_ct), new_data)
})

test_that("data_table<-() correctly adds a 0 to the index vector",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort") |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row()
    new_df <- num_test_df()
    new_data <- crosstab_data(new_df, "cohort")

    orig_index <- attr(test_ct, "index")
    data_table(test_ct) <- new_data
    new_index <- attr(test_ct, "index")

    expect_true(all(orig_index %in% new_index))
    expect_equal(length(orig_index) + 1, length(new_index))
    expect_true(new_index[length(new_index)] == 0)
    expect_equal(sum(orig_index), sum(new_index))
})

test_that("data_table<-() can take a crosstab in value and pass the call onto its data table",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    new_df <- num_test_df()
    new_ct <- crosstab(new_df, "cohort")

    orig_index <- attr(test_ct, "index")

    orig_data <- data_table(test_ct)
    expect_silent(`data_table<-`(test_ct, new_ct))
    data_table(test_ct) <- new_ct
    expect_equal(data_table(test_ct), data_table(new_ct))
})

test_that("set_new_data() works when given a crosstab object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    orig_data <- data_table(test_ct)
    expect_equal(orig_data, crosstab_data(test_df, "cohort"))

    new_df <- cat_test_df()
    new_ct <- crosstab(new_df, "cohort")
    new_data <- data_table(new_ct)

    expect_false(all(new_data == crosstab_data(test_df, "cohort"), na.rm = T))
    expect_silent(set_new_data(test_ct, new_ct))
    test_ct <- set_new_data(test_ct, new_ct)
    expect_equal(new_data, data_table(test_ct))

    new_df <- num_test_df()
    new_ct <- crosstab(new_df, "cohort")
    new_data <- data_table(new_ct)

    expect_false(all(new_data == crosstab_data(test_df, "cohort"), na.rm = T))
    expect_silent(set_new_data(test_ct, new_ct))
    test_ct <- set_new_data(test_ct, new_ct)
    expect_equal(new_data, data_table(test_ct))

    new_df <- lik_test_df()
    new_map <- default_var_map(new_df[["variable"]])
    new_ct <- crosstab(new_df, "cohort", new_map)
    new_data <- data_table(new_ct)

    expect_false(all(as.character(new_data) == as.character(crosstab_data(test_df, "cohort")), na.rm = T))
    expect_silent(set_new_data(test_ct, new_ct))
    test_ct <- set_new_data(test_ct, new_ct)
    expect_equal(new_data, data_table(test_ct))

    new_df <- multi_test_df()
    new_ct <- crosstab(new_df, "cohort")
    new_data <- data_table(new_ct)

    expect_false(all(new_data == crosstab_data(test_df, "cohort"), na.rm = T))
    expect_silent(set_new_data(test_ct, new_ct))
    test_ct <- set_new_data(test_ct, new_ct)
    expect_equal(new_data, data_table(test_ct))
})

test_that("set_new_data() works when given a crosstab_data object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    orig_data <- data_table(test_ct)
    expect_equal(orig_data, crosstab_data(test_df, "cohort"))

    new_df <- cat_test_df()
    new_data <- crosstab_data(new_df, "cohort")

    expect_false(all(new_data == orig_data, na.rm = T))
    expect_silent(set_new_data(test_ct, new_data))
    test_ct <- set_new_data(test_ct, new_data)
    expect_equal(new_data, data_table(test_ct))

    new_df <- num_test_df()
    new_data <- crosstab_data(new_df, "cohort")

    expect_false(all(new_data == orig_data, na.rm = T))
    expect_silent(set_new_data(test_ct, new_data))
    test_ct <- set_new_data(test_ct, new_data)
    expect_equal(new_data, data_table(test_ct))

    new_df <- lik_test_df()
    new_map <- default_var_map(new_df[["variable"]])
    new_data <- crosstab_data(new_df, "cohort", new_map)

    expect_false(all(as.character(new_data) == as.character(orig_data), na.rm = T))
    expect_silent(set_new_data(test_ct, new_data))
    test_ct <- set_new_data(test_ct, new_data)
    expect_equal(new_data, data_table(test_ct))

    new_df <- multi_test_df()
    new_data <- crosstab_data(new_df, "cohort")

    expect_false(all(new_data == orig_data, na.rm = T))
    expect_silent(set_new_data(test_ct, new_data))
    test_ct <- set_new_data(test_ct, new_data)
    expect_equal(new_data, data_table(test_ct))
})

test_that("set_new_data() works when given a data.frame object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    orig_data <- data_table(test_ct)
    expect_equal(orig_data, crosstab_data(test_df, "cohort"))

    new_df <- cat_test_df()

    expect_false(all(test_df == new_df, na.rm = T))
    expect_silent(set_new_data(test_ct, new_df))
    test_ct <- set_new_data(test_ct, new_df)
    expect_equal(crosstab_data(new_df, "cohort"), data_table(test_ct))

    new_df <- num_test_df()

    expect_false(all(test_df == new_df, na.rm = T))
    expect_silent(set_new_data(test_ct, new_df))
    test_ct <- set_new_data(test_ct, new_df)
    expect_equal(crosstab_data(new_df, "cohort"), data_table(test_ct))

    new_df <- lik_test_df()
    new_map <- default_var_map(new_df[["variable"]])

    expect_false(all(as.character(test_df) == as.character(new_df), na.rm = T))
    expect_silent(set_new_data(test_ct, new_df, var_map = new_map))
    test_ct <- set_new_data(test_ct, new_df, var_map = new_map)
    expect_equal(crosstab_data(new_df, "cohort", new_map), data_table(test_ct))

    new_df <- multi_test_df()

    expect_false(all(test_df == new_df, na.rm = T))
    expect_silent(set_new_data(test_ct, new_df))
    test_ct <- set_new_data(test_ct, new_df)
    expect_equal(crosstab_data(new_df, "cohort"), data_table(test_ct))
})

test_that("index<-() sets the new index attribute",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort") |>
        add_total_row() |>
        add_total_row() |>
        add_total_row() |>
        add_total_row()

    expect_equal(index(test_ct), c("variable" = 4))
    expect_silent(`index<-`(test_ct, c(a = 2, b = 2)))
    index(test_ct) <- c(a = 2, b = 2)
    expect_equal(index(test_ct), c(a = 2, b = 2))
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

# UTILITIES ####
test_that("stack_crosstabs() works when given proper data",{
    test_df1 <- cat_test_df(col_name = "cat")
    test_df2 <- num_test_df(col_name = "num")
    test_df3 <- lik_test_df(col_name = "lik")
    test_df4 <- multi_test_df(col_name = "mul")

    test_map <- default_var_map(test_df3[["lik"]])

    expect_silent(
        stack_crosstabs(
            crosstab(test_df1, "cohort") |> add_default_table(),
            crosstab(test_df2, "cohort") |> add_default_table(),
            crosstab(test_df3, "cohort") |> add_default_table(),
            crosstab(test_df4, "cohort") |> add_default_table()
        )
    )

    result <- stack_crosstabs(
        crosstab(test_df1, "cohort") |> add_default_table(),
        crosstab(test_df2, "cohort") |> add_default_table(),
        crosstab(test_df3, "cohort") |> add_default_table(),
        crosstab(test_df4, "cohort") |> add_default_table()
    )

    expect_equal(ncol(result), 7)
    expect_equal(nrow(result), 21)
    expect_equal(index(result), c(cat = 5, num = 3, lik = 6, mul = 7))
})
