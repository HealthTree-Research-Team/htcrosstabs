# new_crosstab_data() ####
test_that("new_crosstab_data() works when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = F) |> add_group_col(value = "All")

    expect_silent(new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        var_levels = categorical_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_CAT,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        var_levels = categorical_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_CAT,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_CAT)
    expect_s3_class(ct_data, "data.frame")
    expect_false(is.crosstab.grouped(ct_data))

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_equal(attr(ct_data, "var_levels"), categorical_levels)
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_null(attr(ct_data, "combined_cohort_name"))
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given grouped categorical data",{
    expect_silent(new_crosstab_data(
        df = cat_test_df(),
        var_col_name = "variable",
        var_levels = categorical_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_CAT,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = cat_test_df(),
        var_col_name = "variable",
        var_levels = categorical_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_CAT,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_CAT)
    expect_s3_class(ct_data, CT_DATA_CLASS_GROUPED)
    expect_s3_class(ct_data, "data.frame")

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_equal(attr(ct_data, "var_levels"), categorical_levels)
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_equal(attr(ct_data, "combined_cohort_name"), "All")
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given ungrouped numeric data",{
    test_df <- num_test_df(gr = F) |> add_group_col(value = "All")

    expect_silent(new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_NUM,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_NUM,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_NUM)
    expect_s3_class(ct_data, "data.frame")
    expect_false(is.crosstab.grouped(ct_data))

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_null(attr(ct_data, "var_levels"))
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_null(attr(ct_data, "combined_cohort_name"))
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given grouped numeric data",{
    expect_silent(new_crosstab_data(
        df = num_test_df(),
        var_col_name = "variable",
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_NUM,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = num_test_df(),
        var_col_name = "variable",
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_NUM,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_NUM)
    expect_s3_class(ct_data, CT_DATA_CLASS_GROUPED)
    expect_s3_class(ct_data, "data.frame")

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_null(attr(ct_data, "var_levels"))
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_equal(attr(ct_data, "combined_cohort_name"), "All")
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given ungrouped likert data",{
    test_df <- lik_test_df(gr = F) |> add_group_col(value = "All")
    test_map <- default_var_map(test_df[["variable"]])

    expect_silent(new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        var_levels = likert_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        var_map = test_map,
        subclass = CT_DATA_CLASS_LIKERT,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        var_levels = likert_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        var_map = test_map,
        subclass = CT_DATA_CLASS_LIKERT,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_LIKERT)
    expect_s3_class(ct_data, "data.frame")
    expect_false(is.crosstab.grouped(ct_data))

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_equal(attr(ct_data, "var_levels"), likert_levels)
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_null(attr(ct_data, "combined_cohort_name"))
    expect_equal(attr(ct_data, "var_map"), test_map)
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given grouped likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])

    expect_silent(new_crosstab_data(
        df = lik_test_df(),
        var_col_name = "variable",
        var_levels = likert_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        var_map = test_map,
        subclass = CT_DATA_CLASS_LIKERT,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = num_test_df(),
        var_col_name = "variable",
        var_levels = likert_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        var_map = test_map,
        subclass = CT_DATA_CLASS_LIKERT,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_LIKERT)
    expect_s3_class(ct_data, CT_DATA_CLASS_GROUPED)
    expect_s3_class(ct_data, "data.frame")

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_equal(attr(ct_data, "var_levels"), likert_levels)
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_equal(attr(ct_data, "combined_cohort_name"), "All")
    expect_equal(attr(ct_data, "var_map"), test_map)
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = F) |> add_group_col(value = "All")

    expect_silent(new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        var_levels = multianswer_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_MULTI,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = test_df,
        var_col_name = "variable",
        var_levels = multianswer_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_MULTI,
        grouped = F,
        combined_cohort_name = "All", # This should be ignored
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_MULTI)
    expect_s3_class(ct_data, "data.frame")
    expect_false(is.crosstab.grouped(ct_data))

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_equal(attr(ct_data, "var_levels"), multianswer_levels)
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_null(attr(ct_data, "combined_cohort_name"))
    expect_null(attr(ct_data, "var_map"))
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

test_that("new_crosstab_data() works when given grouped multianswer data",{
    expect_silent(new_crosstab_data(
        df = multi_test_df(),
        var_col_name = "variable",
        var_levels = multianswer_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_MULTI,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    ))

    ct_data <- new_crosstab_data(
        df = multi_test_df(),
        var_col_name = "variable",
        var_levels = multianswer_levels,
        cohort_col_name = "cohort",
        cohort_levels = character_levels,
        subclass = CT_DATA_CLASS_MULTI,
        grouped = T,
        combined_cohort_name = "All",
        desc_col_name = "Description"
    )

    expect_s3_class(ct_data, CT_DATA_CLASS)
    expect_s3_class(ct_data, CT_DATA_CLASS_MULTI)
    expect_s3_class(ct_data, CT_DATA_CLASS_GROUPED)
    expect_s3_class(ct_data, "data.frame")

    expect_equal(attr(ct_data, "var_col_name"), "variable")
    expect_equal(attr(ct_data, "var_levels"), multianswer_levels)
    expect_equal(attr(ct_data, "cohort_col_name"), "cohort")
    expect_equal(attr(ct_data, "cohort_levels"), character_levels)
    expect_equal(attr(ct_data, "combined_cohort_name"), "All")
    expect_null(attr(ct_data, "var_map"))
    expect_equal(attr(ct_data, "desc_col_name"), "Description")
})

# crosstab_data() ####
test_that("crosstab_data() works when given ungrouped categorical data",{
    test_df <- cat_test_df(gr = F)
    expect_silent(crosstab_data(test_df))
    result <- crosstab_data(test_df)
    expect_s3_class(result, CT_DATA_CLASS_CAT)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, "data.frame")
    expect_false(is.crosstab.grouped(result))
})

test_that("crosstab_data() works when given grouped categorical data",{
    test_df <- cat_test_df()
    expect_silent(crosstab_data(test_df, "cohort"))
    result <- crosstab_data(test_df, "cohort")
    expect_s3_class(result, CT_DATA_CLASS_CAT)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, CT_DATA_CLASS_GROUPED)
    expect_s3_class(result, "data.frame")
})

test_that("crosstab_data() works when given ungrouped numeric data",{
    test_df <- num_test_df(gr = F)
    expect_silent(crosstab_data(test_df))
    result <- crosstab_data(test_df)
    expect_s3_class(result, CT_DATA_CLASS_NUM)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, "data.frame")
    expect_false(is.crosstab.grouped(result))
})

test_that("crosstab_data() works when given grouped numeric data",{
    test_df <- num_test_df()
    expect_silent(crosstab_data(test_df, "cohort"))
    result <- crosstab_data(test_df, "cohort")
    expect_s3_class(result, CT_DATA_CLASS_NUM)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, CT_DATA_CLASS_GROUPED)
    expect_s3_class(result, "data.frame")
})

test_that("crosstab_data() works when given ungrouped likert data",{
    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    expect_silent(crosstab_data(test_df, var_map = test_map))
    result <- crosstab_data(test_df, var_map = test_map)
    expect_s3_class(result, CT_DATA_CLASS_LIKERT)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, "data.frame")
    expect_false(is.crosstab.grouped(result))
})

test_that("crosstab_data() works when given grouped likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    expect_silent(crosstab_data(test_df, "cohort", var_map = test_map))
    result <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_s3_class(result, CT_DATA_CLASS_LIKERT)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, CT_DATA_CLASS_GROUPED)
    expect_s3_class(result, "data.frame")
})

test_that("crosstab_data() works when given ungrouped multianswer data",{
    test_df <- multi_test_df(gr = F)
    expect_silent(crosstab_data(test_df))
    result <- crosstab_data(test_df)
    expect_s3_class(result, CT_DATA_CLASS_MULTI)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, "data.frame")
    expect_false(is.crosstab.grouped(result))
})

test_that("crosstab_data() works when given grouped multianswer data",{
    test_df <- multi_test_df()
    expect_silent(crosstab_data(test_df, "cohort"))
    result <- crosstab_data(test_df, "cohort")
    expect_s3_class(result, CT_DATA_CLASS_MULTI)
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, CT_DATA_CLASS_GROUPED)
    expect_s3_class(result, "data.frame")
})

# GETTERS ####
test_that("var_name() returns correct value", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(var_name(test_ct_data), "variable")
})

test_that("var() returns correct column", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(var(test_ct_data), test_ct_data[["variable"]])
})

test_that("var_levels() returns correct value", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(var_levels(test_ct_data), levels(test_ct_data[["variable"]]))
})

test_that("cohort_name() returns correct value", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(cohort_name(test_ct_data), "cohort")
})

test_that("cohort() returns correct column", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(cohort(test_ct_data), test_ct_data[["cohort"]])
})

test_that("cohort_levels() returns correct value", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(cohort_levels(test_ct_data), levels(test_ct_data[["cohort"]]))
})

test_that("var_map() returns correct mapping", {
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct_data <- crosstab_data(test_df, "cohort", test_map)
    expect_s3_class(test_ct_data, CT_DATA_CLASS_LIKERT)
    expect_equal(var_map(test_ct_data), test_map)
})

test_that("var_mapped() maps values correctly", {
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct_data <- crosstab_data(test_df, "cohort", test_map)
    expect_s3_class(test_ct_data, CT_DATA_CLASS_LIKERT)
    expect_equal(var_mapped(test_ct_data), test_map[match(test_ct_data[["variable"]], names(test_map))])
})

test_that("get_raw_data() returns the grouped data without the \"all\" values",{
    test_df <- cat_test_df()
    test_ct_data <- crosstab_data(test_df, "cohort")
    expect_false("All" %in% cohort(get_raw_data(test_ct_data)))
})

test_that("get_raw_data() returns ungrouped data unchanged",{
    test_df <- cat_test_df(gr = F)
    test_ct_data <- crosstab_data(test_df)
    expect_equal(test_df[["variable"]], var(get_raw_data(test_ct_data)))
})

test_that("desc_name() returns the correct description name",{
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(desc_name(test_ct_data), "Description")
})

# SETTERS ####
test_that("var_name<- updates column name and attribute", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_equal(attr(test_ct_data, "var_col_name"), "variable")
    expect_silent(`var_name<-`(test_ct_data, "new_variable"))
    var_name(test_ct_data) <- "new_variable"
    expect_equal(var_name(test_ct_data), "new_variable")
    expect_true("new_variable" %in% names(test_ct_data))
})

test_that("var<- replaces column values correctly", {
    shuffle_vals <- function(col) {
        last_val <- col[length(col)]
        all_but_last_val <- col[1:(length(col)-1)]
        new_col <- c(last_val, all_but_last_val)
        new_col
    }

    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    orig_var_col <- test_ct_data[["variable"]]
    new_var_col <- shuffle_vals(orig_var_col)

    expect_false(identical(orig_var_col, new_var_col))
    expect_silent(`var<-`(test_ct_data, new_var_col))
    var(test_ct_data) <- new_var_col
    expect_equal(test_ct_data[["variable"]], new_var_col)
})

test_that("var<- updates var_levels", {
    new_levels <- rev(categorical_levels)
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    orig_var_col <- test_ct_data[["variable"]]
    new_var_col <- factor(orig_var_col, levels = new_levels)

    expect_false(identical(categorical_levels, new_levels))
    expect_false(identical(orig_var_col, new_var_col))
    expect_silent(`var<-`(test_ct_data, new_var_col))
    var(test_ct_data) <- new_var_col
    expect_equal(test_ct_data[["variable"]], new_var_col)
    expect_equal(levels(test_ct_data[["variable"]]), new_levels)
})

test_that("var_levels<- updates attribute correctly", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    new_levels <- rev(categorical_levels)

    expect_equal(levels(test_ct_data[["variable"]]), categorical_levels)
    var_levels(test_ct_data) <- new_levels
    expect_equal(var_levels(test_ct_data), new_levels)
})

test_that("var_levels<- updates levels of var() correctly", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    new_levels <- rev(categorical_levels)

    var_levels(test_ct_data) <- new_levels
    expect_equal(levels(var(test_ct_data)), new_levels)
})

test_that("cohort_name<- updates column name and attribute", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    cohort_name(test_ct_data) <- "group"
    expect_equal(cohort_name(test_ct_data), "group")
    expect_true("group" %in% names(test_ct_data))
})

test_that("cohort<- replaces values correctly", {
    shuffle_vals <- function(col) {
        last_val <- col[length(col)]
        all_but_last_val <- col[1:(length(col)-1)]
        new_col <- c(last_val, all_but_last_val)
        new_col
    }

    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    orig_cohort_col <- test_ct_data[["cohort"]]
    new_cohort_col <- shuffle_vals(orig_cohort_col)

    expect_false(identical(orig_cohort_col, new_cohort_col))
    expect_silent(`cohort<-`(test_ct_data, new_cohort_col))
    cohort(test_ct_data) <- new_cohort_col
    expect_equal(test_ct_data[["cohort"]], new_cohort_col)
})

test_that("cohort<- updates cohort_levels correctly",{
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    orig_cohort_col <- test_ct_data[["cohort"]]
    orig_levels <- levels(orig_cohort_col)
    new_levels <- rev(orig_levels)
    new_cohort_col <- factor(orig_cohort_col, levels = new_levels)

    expect_silent(`cohort<-`(test_ct_data, new_cohort_col))
    cohort(test_ct_data) <- new_cohort_col
    expect_equal(levels(test_ct_data[["cohort"]]), new_levels)
})

test_that("cohort_levels<- updates attribute correctly", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    new_levels <- rev(levels(test_ct_data[["cohort"]]))

    expect_silent(`cohort_levels<-`(test_ct_data, new_levels))
    cohort_levels(test_ct_data) <- new_levels
    expect_equal(cohort_levels(test_ct_data), new_levels)
})

test_that("cohort_levels<- updates levels of cohort() correctly", {
    test_ct_data <- crosstab_data(cat_test_df(), "cohort")
    new_levels <- rev(levels(test_ct_data[["cohort"]]))

    expect_silent(`cohort_levels<-`(test_ct_data, new_levels))
    cohort_levels(test_ct_data) <- new_levels
    expect_equal(levels(test_ct_data[["cohort"]]), new_levels)
})

test_that("var_map<- updates mapping correctly", {
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct_data <- crosstab_data(test_df, "cohort", test_map)
    new_map <- test_map
    names(new_map) <- rev(names(new_map))

    expect_silent(`var_map<-`(test_ct_data, new_map))
    var_map(test_ct_data) <- new_map
    expect_equal(var_map(test_ct_data), new_map)
})
