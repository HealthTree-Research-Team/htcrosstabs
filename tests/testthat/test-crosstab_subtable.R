test_that("new_crosstab_sub() works with correct input", {
    df <- get_numeric_test_df() |> add_group_col()
    expect_no_error(new_crosstab_sub(df, "variable", "cohort", NULL, "Response"))
    df <- get_numeric_test_df(gr = T, group_type = "c")
    expect_no_error(new_crosstab_sub(df, "variable", "cohort", NULL, character_levels))
    df <- get_numeric_test_df(gr = T, group_type = "n")
    expect_no_error(new_crosstab_sub(df, "variable", "cohort", NULL, numeric_levels))

    df <- get_categorical_test_df() |> add_group_col()
    expect_no_error(new_crosstab_sub(df, "variable", "cohort", NULL, "Response"))
    df <- get_categorical_test_df(gr = T, group_type = "c")
    expect_no_error(new_crosstab_sub(df, "variable", "cohort", categorical_levels, character_levels))
    df <- get_categorical_test_df(gr = T, group_type = "n")
    expect_no_error(new_crosstab_sub(df, "variable", "cohort", categorical_levels, numeric_levels))
})

test_that("new_crosstab_sub_multi() works with correct input", {
    df <- get_multianswer_test_df() |> add_group_col()
    expect_no_error(new_crosstab_sub_multi(df, "variable", "cohort", NULL, "Response"))
    df <- get_multianswer_test_df(gr = T, group_type = "c")
    expect_no_error(new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, character_levels))
    df <- get_multianswer_test_df(gr = T, group_type = "n")
    expect_no_error(new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, numeric_levels))
})

test_that("new_crosstab_sub() adds CT_SUB_CLASS for nominal data", {
    df <- get_numeric_test_df() |> add_group_col()
    result <- new_crosstab_sub(df, "variable", "cohort", NULL, character_levels)
    expect_s3_class(result, CT_SUB_CLASS)
})

test_that("new_crosstab_sub_multi() adds CT_SUB_CLASS for multiple response data", {
    df <- get_multianswer_test_df() |> add_group_col()
    result <- new_crosstab_sub_multi(df, "variable", "cohort", NULL, character_levels)
    expect_s3_class(result, CT_SUB_CLASS)
})

test_that("new_crosstab_sub_multi() adds CT_SUB_MUL_CLASS for multiple response data", {
    df <- get_multianswer_test_df() |> add_group_col()
    result <- new_crosstab_sub_multi(df, "variable", "cohort", NULL, character_levels)
    expect_s3_class(result, CT_SUB_MUL_CLASS)
})

test_that("new_crosstab_sub() errors when df is not a data.frame", {
    expect_error(new_crosstab_sub(1, "variable", "cohort", categorical_levels, numeric_levels))
    expect_error(new_crosstab_sub(NULL, "variable", "cohort", categorical_levels, numeric_levels))
})

test_that("new_crosstab_sub() errors when variable is invalid", {
    expect_error(new_crosstab_sub(get_test_df(), 1, "cohort", categorical_levels, numeric_levels))
    expect_error(new_crosstab_sub(get_test_df(), NULL, "cohort", categorical_levels, numeric_levels))
    expect_error(new_crosstab_sub(get_test_df(), c("a", "b"), "cohort", categorical_levels, numeric_levels))
})

test_that("new_crosstab_sub() errors when cohort is invalid", {
    expect_error(new_crosstab_sub(get_test_df(), "variable", 1, c("a", "b"), c("x", "y")))
    expect_error(new_crosstab_sub(get_test_df(), "variable", NULL, c("a", "b"), c("x", "y")))
    expect_error(new_crosstab_sub(get_test_df(), "variable", c("a", "b"), c("a", "b"), c("x", "y")))
})

test_that("new_crosstab_sub() errors when cohort_levels is NULL", {
    expect_error(new_crosstab_sub(get_test_df(), "variable", "cohort", c("a", "b"), NULL))
})

test_that("validate_crosstab_sub() works for proper nominal object", {
    df <- get_numeric_test_df() |> add_group_col() |> factorize_columns()
    obj <- new_crosstab_sub(df, "variable", "cohort", NULL, "Response")
    expect_true(validate_crosstab_sub(obj))

    df <- get_numeric_test_df(gr = T, group_type = "c") |> factorize_columns()
    obj <- new_crosstab_sub(df, "variable", "cohort", NULL, character_levels)
    expect_true(validate_crosstab_sub(obj))

    df <- get_numeric_test_df(gr = T, group_type = "n") |> factorize_columns()
    obj <- new_crosstab_sub(df, "variable", "cohort", NULL, numeric_levels)
    expect_true(validate_crosstab_sub(obj))

    df <- get_categorical_test_df() |> add_group_col() |> factorize_columns()
    obj <- new_crosstab_sub(df, "variable", "cohort", categorical_levels, "Response")
    expect_true(validate_crosstab_sub(obj))

    df <- get_categorical_test_df(gr = T, group_type = "c") |> factorize_columns()
    obj <- new_crosstab_sub(df, "variable", "cohort", categorical_levels, character_levels)
    expect_true(validate_crosstab_sub(obj))

    df <- get_categorical_test_df(gr = T, group_type = "n") |> factorize_columns()
    obj <- new_crosstab_sub(df, "variable", "cohort", categorical_levels, numeric_levels)
    expect_true(validate_crosstab_sub(obj))

    df <- get_multianswer_test_df() |> add_group_col() |> factorize_columns()
    obj <- new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, "Response")
    expect_true(validate_crosstab_sub(obj))

    df <- get_multianswer_test_df(gr = T, group_type = "c") |> factorize_columns()
    obj <- new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, character_levels)
    expect_true(validate_crosstab_sub(obj))

    df <- get_multianswer_test_df(gr = T, group_type = "n") |> factorize_columns()
    obj <- new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, numeric_levels)
    expect_true(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() errors if df has != 2 columns", {
    df <- data.frame(variable = factor("a"))
    obj <- new_crosstab_sub(df, "variable", "cohort", c("a"), c("x"))
    expect_error(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() errors if variable not in df", {
    df <- data.frame(foo = factor("a"), cohort = factor("x"))
    obj <- new_crosstab_sub(df, "variable", "cohort", c("a"), c("x"))
    expect_error(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() errors on character columns", {
    df <- get_categorical_test_df(gr = T)
    obj <- new_crosstab_sub(df, "variable", "cohort", categorical_levels, character_levels)
    expect_error(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() checks for list-column in multiple answer data", {
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, character_levels)
    expect_true(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() errors on characters in list-columns", {
    df <- get_multianswer_test_df() |> add_group_col()
    obj <- new_crosstab_sub_multi(df, "variable", "cohort", multianswer_levels, character_levels)
    expect_error(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() fails if multiple-answer variable not list-column", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_sub_multi(df, "variable", "cohort", categorical_levels, character_levels)
    expect_error(validate_crosstab_sub(obj))
})

test_that("validate_crosstab_sub() errors if variable values not in variable_levels", {
    df <- get_multianswer_test_df(gr = T, nrows = 200) |> factorize_columns()
    incomplete_levels <- multianswer_levels[1:(length(multianswer_levels)-1)]
    obj <- new_crosstab_sub(df, "variable", "cohort", incomplete_levels, character_levels)
    expect_error(validate_crosstab_sub(obj))
})

test_that("crosstab_sub() throws no errors on correct data",{
    df <- get_categorical_test_df()
    expect_no_error(crosstab_sub(df))

    df <- get_categorical_test_df(gr = T)
    expect_no_error(crosstab_sub(df, cohort_col_name = "cohort"))

    df <- get_numeric_test_df()
    expect_no_error(crosstab_sub(df))

    df <- get_numeric_test_df(gr = T)
    expect_no_error(crosstab_sub(df, cohort_col_name = "cohort"))

    df <- get_multianswer_test_df()
    expect_no_error(crosstab_sub(df))

    df <- get_multianswer_test_df(gr = T)
    expect_no_error(crosstab_sub(df, cohort_col_name = "cohort"))
})

test_that("crosstab_sub() errors if df is not a data.frame",{
    expect_error(crosstab_sub(1))
    expect_error(crosstab_sub(NULL))
    expect_error(crosstab_sub("a"))
    expect_error(crosstab_sub(TRUE))
    expect_error(crosstab_sub(FALSE))
    expect_error(crosstab_sub(list()))
})

test_that("crosstab_sub() errors if cohort_col_name is not in the df",{
    expect_error(crosstab_sub(get_categorical_test_df(), cohort_col_name = "test"))
    expect_error(crosstab_sub(get_categorical_test_df(gr = T), cohort_col_name = "test"))
    expect_error(crosstab_sub(get_categorical_test_df(), cohort_col_name = 0))
    expect_error(crosstab_sub(get_categorical_test_df(gr = T), cohort_col_name = 0))
    expect_error(crosstab_sub(get_categorical_test_df(), cohort_col_name = factor("test")))
    expect_error(crosstab_sub(get_categorical_test_df(gr = T), cohort_col_name = factor("test")))
})

test_that("crosstab_sub() throws a warning if it detects a column of all NA values",{
    df <- get_categorical_test_df(gr = T)
    df[["cohort"]] <- c(NA)
    expect_warning(crosstab_sub(df, cohort_col_name = "cohort"))
})

test_that("crosstab_sub() adds a grouping column if df only has one column",{
    df <- get_categorical_test_df()
    ct_sub <- crosstab_sub(df)
    expect_gt(ncol(ct_sub), ncol(df))
})

test_that("crosstab_sub() errors if user provides a two column df but doesn't specify which is the cohort column",{
    expect_error(crosstab_sub(get_categorical_test_df(gr = T)))
})

test_that("crosstab_sub() errors if user provides the name of the cohort column but only provides one column in df",{
    expect_error(crosstab_sub(get_categorical_test_df(), cohort_col_name = "test"))
})

test_that("crosstab_sub() only accepts 1 or 2 columns",{
    # 0 Columns (fail)
    expect_error(crosstab_sub(data.frame()))

    # 1 Column (pass)
    expect_no_error(crosstab_sub(get_categorical_test_df()))

    # 2 Columns (pass)
    expect_no_error(crosstab_sub(get_categorical_test_df(gr = T), cohort_col_name = "cohort"))

    # 3 Columns (fail)
    df <- get_categorical_test_df(gr = T)
    df[["col3"]] = 3
    expect_error(crosstab_sub(df))
    expect_error(crosstab_sub(df, cohort_col_name = "cohort"))

    # 4 Columns (fail)
    df[["col4"]] = 4
    expect_error(crosstab_sub(df))
    expect_error(crosstab_sub(df, cohort_col_name = "cohort"))

    # 5 Columns (fail)
    df[["col5"]] = 5
    expect_error(crosstab_sub(df))
    expect_error(crosstab_sub(df, cohort_col_name = "cohort"))
})

test_that("crosstab_sub() coerces character columns to factors",{
    df <- get_categorical_test_df()
    ct_sub <- crosstab_sub(df)
    expect_false(is.factor(df[["variable"]]))
    expect_true(is.factor(ct_sub[["variable"]]))
})

test_that("crosstab_sub() coerces logical columns to factors",{
    df <- data.frame(variable = rep(c(TRUE, FALSE, NA), 3))
    ct_sub <- crosstab_sub(df)
    expect_false(is.factor(df[["variable"]]))
    expect_true(is.factor(ct_sub[["variable"]]))
})

test_that("crosstab_sub() doesn't coerce numeric columns to factors",{
    df <- get_numeric_test_df()
    ct_sub <- crosstab_sub(df)
    expect_false(is.factor(df[["variable"]]))
    expect_false(is.factor(ct_sub[["variable"]]))
})

test_that("crosstab_sub() adds CT_SUB_CLASS for nominal data", {
    df <- get_numeric_test_df()
    ct_sub <- crosstab_sub(df)
    expect_s3_class(ct_sub, CT_SUB_CLASS)
})

test_that("crosstab_sub() adds CT_SUB_CLASS for multiple response data", {
    df <- get_multianswer_test_df()
    ct_sub <- crosstab_sub(df)
    expect_s3_class(ct_sub, CT_SUB_CLASS)
})

test_that("crosstab_sub() adds CT_SUB_MUL_CLASS for multiple response data", {
    df <- get_multianswer_test_df()
    ct_sub <- crosstab_sub(df)
    expect_s3_class(ct_sub, CT_SUB_MUL_CLASS)
})

test_that("Getters error if class is wrong", {
    expect_error(var(data.frame()))
    expect_error(var_name(data.frame()))
    expect_error(var_levels(data.frame()))

    expect_error(cohort(data.frame()))
    expect_error(cohort_name(data.frame()))
    expect_error(cohort_levels(data.frame()))
})

test_that("Setters error if class is wrong", {
    df <- data.frame(x = 1:3, y = letters[1:3])

    expect_error(`var_name<-`(df, "new_var"))
    expect_error(`var<-`(df, c("a", "b", "c")))
    expect_error(`var_levels<-`(df, c("a", "b")))

    expect_error(`cohort_name<-`(df, "new_cohort"))
    expect_error(`cohort<-`(df, c("G1", "G2", "G1")))
    expect_error(`cohort_levels<-`(df, c("G1", "G2")))
})

test_that("var_name() and cohort_name() return expected names",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    expect_equal(var_name(ct_sub), "variable")
    expect_equal(cohort_name(ct_sub), "cohort")
})

test_that("var() and cohort() return expected columns", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    expect_equal(var(ct_sub), df$variable)
    expect_equal(cohort(ct_sub), df$cohort)
})

test_that("var_levels() and cohort_levels() return expected vectors", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    expect_equal(var_levels(ct_sub), levels(df$variable))
    expect_equal(cohort_levels(ct_sub), levels(df$cohort))
})

test_that("var_levels() returns NULL on numerical data",{
    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    expect_null(var_levels(ct_sub))
})

test_that("`var_name<-`() and `cohort_name<-`() successfully change both attributes and column names",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    var_name(ct_sub) <- "test_var_name"
    expect_equal(var_name(ct_sub), "test_var_name")
    expect_in("test_var_name", names(ct_sub))

    cohort_name(ct_sub) <- "test_cohort_name"
    expect_equal(cohort_name(ct_sub), "test_cohort_name")
    expect_in("test_cohort_name", names(ct_sub))
})

test_that("`var<-`() and `cohort<-`() successfully change the column values", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    var(ct_sub) <- factor("variable_test")
    expect_true(all(factor("variable_test") == var(ct_sub)))

    cohort(ct_sub) <- factor("cohort_test")
    expect_true(all(factor("cohort_test") == cohort(ct_sub)))
})

test_that("`var_levels<-`() and `cohort_levels<-`() successfully change both attributes and column levels", {
    correct_var_levels <- c("morning", "afternoon", "evening", "night")
    correct_cohort_levels <- c("A", "B", "C", "D")

    # Reroll if they match right off the bat
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    while(
        identical(levels(df[["variable"]]), correct_var_levels) |
        identical(levels(df[["cohort"]]), correct_cohort_levels) |
        length(levels(df[["variable"]])) != length(correct_var_levels) |
        length(levels(df[["cohort"]])) != length(correct_cohort_levels)
        ) {
        df <- get_categorical_test_df(gr = T) |> factorize_columns()
    }
    ct_sub <- crosstab_sub(df, cohort_col_name = "cohort")

    incorrect_var_levels <- var_levels(ct_sub)
    incorrect_cohort_levels <- cohort_levels(ct_sub)

    # Check var_levels
    expect_false(identical(correct_var_levels, incorrect_var_levels))
    var_levels(ct_sub) <- correct_var_levels
    expect_false(identical(var_levels(ct_sub), incorrect_var_levels))
    expect_equal(var_levels(ct_sub), correct_var_levels)
    expect_equal(var_levels(ct_sub), levels(var(ct_sub)))

    # Check cohort_levels
    expect_false(identical(correct_cohort_levels, incorrect_cohort_levels))
    cohort_levels(ct_sub) <- correct_cohort_levels
    expect_false(identical(cohort_levels(ct_sub), incorrect_cohort_levels))
    expect_equal(cohort_levels(ct_sub), correct_cohort_levels)
    expect_equal(cohort_levels(ct_sub), levels(cohort(ct_sub)))
})
