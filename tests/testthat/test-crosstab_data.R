# CONSTRUCTORS ####
test_that("new_crosstab_data constructs object with correct attributes and classes", {
    df <- data.frame(var = factor(c("A", "B")), cohort = factor(c("X", "Y")))

    result <- new_crosstab_data(
        df,
        var_col_name = "var",
        cohort_col_name = "cohort",
        cohort_levels = c("X", "Y"),
        var_levels = c("A", "B"),
        var_mapping = c(A = 1, B = 2),
        subclass = "crosstab_data_cat"
    )

    expect_s3_class(result, "crosstab_data_cat")
    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, "data.frame")

    expect_equal(attr(result, "var_col_name"), "var")
    expect_equal(attr(result, "cohort_col_name"), "cohort")
    expect_equal(attr(result, "cohort_levels"), c("X", "Y"))
    expect_equal(attr(result, "var_levels"), c("A", "B"))
    expect_equal(attr(result, "var_mapping"), c(A = 1, B = 2))
})

test_that("new_crosstab_data works without optional var_levels, var_mapping, and subclass", {
    df <- data.frame(var = 1:2, cohort = factor(c("X", "Y")))

    result <- new_crosstab_data(
        df,
        var_col_name = "var",
        cohort_col_name = "cohort",
        cohort_levels = c("X", "Y")
    )

    expect_s3_class(result, CT_DATA_CLASS)
    expect_s3_class(result, "data.frame")
    expect_null(attr(result, "var_levels"))
    expect_null(attr(result, "var_mapping"))
})

test_that("new_crosstab_data() doesn't error with correct input",{
    expect_no_error(new_crosstab_data(data.frame(), "variable", "cohort", c("a", "b")))
})

test_that("new_crosstab_data errors with invalid input types", {
    df <- data.frame(var = 1:2, cohort = 3:4)

    expect_error(new_crosstab_data(df, 1, "cohort", c("A")))
    expect_error(new_crosstab_data(df, "var", 2, c("A")))

    expect_error(new_crosstab_data(df, "var", "cohort", c("A"), var_mapping = 1:2))
    expect_error(new_crosstab_data(df, "var", "cohort", c("A"), var_mapping = c("A", "B")), "numeric")
    expect_error(new_crosstab_data(df, "var", "cohort", c("A"), subclass = 1))
})

test_that("new_crosstab_data respects subclass chaining", {
    df <- data.frame(var = 1:2, cohort = factor(c("X", "Y")))

    result <- new_crosstab_data(
        df,
        var_col_name = "var",
        cohort_col_name = "cohort",
        cohort_levels = c("X", "Y"),
        subclass = "crosstab_data_num"
    )

    expect_equal(class(result)[1:2], c("crosstab_data_num", CT_DATA_CLASS))
})

test_that("new_crosstab_data_cat sets correct subclass and attributes", {
    df <- data.frame(var = factor(c("A", "B")), cohort = factor(c("X", "Y")))
    obj <- new_crosstab_data_cat(df, "var", c("A", "B"), "cohort", c("X", "Y"))

    expect_s3_class(obj, CT_DATA_CLASS_CAT)
    expect_equal(attr(obj, "var_col_name"), "var")
    expect_equal(attr(obj, "var_levels"), c("A", "B"))
    expect_equal(attr(obj, "cohort_col_name"), "cohort")
    expect_equal(attr(obj, "cohort_levels"), c("X", "Y"))
})

test_that("new_crosstab_data_num sets correct subclass", {
    df <- data.frame(var = c(1.1, 2.2), cohort = factor(c("X", "Y")))
    obj <- new_crosstab_data_num(df, "var", "cohort", c("X", "Y"))

    expect_s3_class(obj, CT_DATA_CLASS_NUM)
    expect_equal(attr(obj, "var_col_name"), "var")
})

test_that("new_crosstab_data_likert sets correct subclass and var_mapping", {
    df <- data.frame(var = factor(c("Agree", "Neutral")), cohort = factor(c("X", "Y")))
    mapping <- c(Disagree = 1, Neutral = 2, Agree = 3)

    obj <- new_crosstab_data_likert(df, "var", c("Disagree", "Neutral", "Agree"), mapping, "cohort", c("X", "Y"))

    expect_s3_class(obj, CT_DATA_CLASS_LIKERT)
    expect_equal(attr(obj, "var_mapping"), mapping)
})

test_that("new_crosstab_data_multi sets correct subclass", {
    df <- get_multianswer_test_df(gr = T)
    obj <- new_crosstab_data_multi(df, "variable", multianswer_levels, "cohort", character_levels)

    expect_s3_class(obj, CT_DATA_CLASS_MULTI)
    expect_equal(attr(obj, "var_levels"), multianswer_levels)
})

# VALIDATORS ####
test_that("validate_crosstab_data.crosstab_data_cat() passes on valid input", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_cat(df, "variable", categorical_levels, "cohort", character_levels)
    expect_true(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if obj is not of type crosstab_data_cat",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data(df, "variable", "cohort", character_levels, var_levels = categorical_levels)
    expect_false(inherits(obj, CT_DATA_CLASS_CAT))
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if var_col_name is not a column in df",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_cat(df, "test", categorical_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if variable is not a factor", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns("cohort")
    obj <- new_crosstab_data_cat(df, "variable", categorical_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if cohort_col_name is not a column in df",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_cat(df, "variable", categorical_levels, "test", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if cohort is not a factor", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns("variable")
    obj <- new_crosstab_data_cat(df, "variable", categorical_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if variable levels are wrong", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- categorical_levels[2:length(categorical_levels)]
    obj <- new_crosstab_data_cat(df, "variable", incorrect_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails if cohort levels are wrong", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- character_levels[2:length(character_levels)]
    obj <- new_crosstab_data_cat(df, "variable", categorical_levels, "cohort", incorrect_levels)
    expect_error(validate_crosstab_data.crosstab_data_cat(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() passes on valid input", {
    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_num(df, "variable", "cohort", character_levels)
    expect_true(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() fails if obj is not of type crosstab_data_num",{
    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data(df, "variable", "cohort", character_levels)
    expect_false(inherits(obj, CT_DATA_CLASS_NUM))
    expect_error(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() fails if var_col_name is not a column in df",{
    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_num(df, "test", "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() fails if variable is not a numeric", {
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_num(df, "variable", "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() fails if cohort_col_name is not a column in df",{
    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_num(df, "variable", "test", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() fails if cohort is not a factor", {
    df <- get_numeric_test_df(gr = T) |> factorize_columns("variable")
    obj <- new_crosstab_data_num(df, "variable", "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_num() fails if cohort levels are wrong", {
    df <- get_numeric_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- character_levels[2:length(character_levels)]
    obj <- new_crosstab_data_num(df, "variable", "cohort", incorrect_levels)
    expect_error(validate_crosstab_data.crosstab_data_num(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() passes on valid input", {
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    obj <- new_crosstab_data_likert(df, "variable", likert_levels, map, "cohort", character_levels)
    expect_true(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if obj is not of type crosstab_data_likert",{
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data(df, "variable", "cohort", character_levels, var_levels = likert_levels)
    expect_false(inherits(obj, CT_DATA_CLASS_LIKERT))
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if var_col_name is not a column in df",{
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    obj <- new_crosstab_data_likert(df, "test", likert_levels, map, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if variable is not a factor", {
    df <- get_likert_test_df(gr = T) |> factorize_columns("cohort")
    map <- default_likert_map(factor(df[["variable"]]))
    obj <- new_crosstab_data_likert(df, "variable", likert_levels, map, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if cohort_col_name is not a column in df",{
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    obj <- new_crosstab_data_likert(df, "variable", likert_levels, map, "test", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if cohort is not a factor", {
    df <- get_likert_test_df(gr = T) |> factorize_columns("variable")
    map <- default_likert_map(df[["variable"]])
    obj <- new_crosstab_data_likert(df, "variable", likert_levels, map, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if variable levels are wrong", {
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- likert_levels[2:length(likert_levels)]
    map <- default_likert_map(df[["variable"]])
    obj <- new_crosstab_data_likert(df, "variable", incorrect_levels, map, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if cohort levels are wrong", {
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- character_levels[2:length(character_levels)]
    map <- default_likert_map(df[["variable"]])
    obj <- new_crosstab_data_likert(df, "variable", likert_levels, map, "cohort", incorrect_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails if map doesn't map every value",{
    df <- get_likert_test_df(gr = T) |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    map <- map[2:length(map)]
    obj <- new_crosstab_data_likert(df, "variable", likert_levels, map, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_likert(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() passes on valid input", {
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_multi(df, "variable", multianswer_levels, "cohort", character_levels)
    expect_true(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if obj is not of type crosstab_data_multi",{
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data(df, "variable", "cohort", character_levels, var_levels = multianswer_levels)
    expect_false(inherits(obj, CT_DATA_CLASS_MULTI))
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if var_col_name is not a column in df",{
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_multi(df, "test", multianswer_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if variable is not a factorlist", {
    df <- get_multianswer_test_df(gr = T) |> factorize_columns("cohort")
    obj <- new_crosstab_data_multi(df, "variable", multianswer_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if cohort_col_name is not a column in df",{
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    obj <- new_crosstab_data_multi(df, "variable", multianswer_levels, "test", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if cohort is not a factor", {
    df <- get_multianswer_test_df(gr = T) |> factorize_columns("variable")
    obj <- new_crosstab_data_multi(df, "variable", multianswer_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if variable levels are wrong", {
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- multianswer_levels[2:length(multianswer_levels)]
    obj <- new_crosstab_data_multi(df, "variable", incorrect_levels, "cohort", character_levels)
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails if cohort levels are wrong", {
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    incorrect_levels <- character_levels[2:length(character_levels)]
    obj <- new_crosstab_data_multi(df, "variable", multianswer_levels, "cohort", incorrect_levels)
    expect_error(validate_crosstab_data.crosstab_data_multi(obj))
})

# HELPERS ####
test_that("crosstab_data() works when provided valid ungrouped categorical data",{
    df <- get_categorical_test_df()
    expect_warning(crosstab_data(df))

    df <- get_categorical_test_df() |> factorize_columns()
    expect_no_warning(crosstab_data(df))
})

test_that("crosstab_data() works when provided valid ungrouped numeric data",{
    df <- get_numeric_test_df()
    expect_no_error(crosstab_data(df))
    expect_no_warning(crosstab_data(df))
})

test_that("crosstab_data() works when provided valid ungrouped likert data",{
    df <- get_likert_test_df()
    map <- default_likert_map(factor(df[["variable"]]))
    expect_warning(crosstab_data(df, likert_map = map))

    df <- get_likert_test_df() |> factorize_columns()
    map <- default_likert_map(factor(df[["variable"]]))
    expect_no_warning(crosstab_data(df, likert_map = map))
})

test_that("crosstab_data() works when provided valid ungrouped multianswer data",{
    df <- get_multianswer_test_df()
    expect_warning(crosstab_data(df))

    df <- get_multianswer_test_df() |> factorize_columns()
    expect_no_warning(crosstab_data(df))
})

test_that("crosstab_data() works when provided valid grouped categorical data",{
    df <- get_categorical_test_df(gr = T)
    expect_warning(expect_warning(crosstab_data(df, "cohort")))

    df <- get_categorical_test_df(gr = T) |> factorize_columns("variable")
    expect_warning(crosstab_data(df, "cohort"))

    df <- get_categorical_test_df(gr = T) |> factorize_columns("cohort")
    expect_warning(crosstab_data(df, "cohort"))

    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_no_warning(crosstab_data(df, "cohort"))
})

test_that("crosstab_data() works when provided valid grouped numeric data",{
    df <- get_numeric_test_df(gr = T)
    expect_warning(crosstab_data(df, "cohort"))

    df <- get_numeric_test_df(gr = T) |> factorize_columns("cohort")
    expect_no_warning(crosstab_data(df, "cohort"))
})

test_that("crosstab_data() works when provided valid grouped likert data",{
    df <- get_likert_test_df(gr = T)
    map <- default_likert_map(factor(df[["variable"]]))
    expect_warning(expect_warning(crosstab_data(df, "cohort", likert_map = map)))

    df <- get_likert_test_df(gr = T) |> factorize_columns("variable")
    expect_warning(crosstab_data(df, "cohort", likert_map = map))

    df <- get_likert_test_df(gr = T) |> factorize_columns("cohort")
    expect_warning(crosstab_data(df, "cohort", likert_map = map))

    df <- get_likert_test_df(gr = T) |> factorize_columns()
    expect_no_warning(crosstab_data(df, "cohort", likert_map = map))
})

test_that("crosstab_data() works when provided valid grouped multianswer data",{
    df <- get_multianswer_test_df(gr = T)
    expect_warning(expect_warning(crosstab_data(df, "cohort")))

    df <- get_multianswer_test_df(gr = T) |> factorize_columns("variable")
    expect_warning(crosstab_data(df, "cohort"))

    df <- get_multianswer_test_df(gr = T) |> factorize_columns("cohort")
    expect_warning(crosstab_data(df, "cohort"))

    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    expect_no_warning(crosstab_data(df, "cohort"))
})

test_that("crosstab_data() errors when df isn't a data.frame", {
    expect_error(crosstab_data(1))
    expect_error(crosstab_data("a"))
    expect_error(crosstab_data(NA))
    expect_error(crosstab_data(NULL))
})

test_that("crosstab_data() errors when cohort_col_name is left NULL, but df has some number of columns other than 1",{
    # 0 columns (fail)
    df <- data.frame() |> factorize_columns()
    expect_error(crosstab_data(df))

    # 1 column (pass)
    df <- get_categorical_test_df() |> factorize_columns()
    expect_no_error(crosstab_data(df))

    # 2 columns (fail)
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_error(crosstab_data(df))

    # 3 columns (fail)
    df[["test3"]] <- 3
    expect_error(crosstab_data(df))

    # 4 columns (fail)
    df[["test4"]] <- 4
    expect_error(crosstab_data(df))
})

test_that("crosstab_data() creates grouping column called cohort for ungrouped data",{
    df <- get_categorical_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_equal(cohort_name(obj), "cohort")
})

test_that("crosstab_data() creates unused grouping column when ungrouped data column is already called cohort",{
    df <- get_categorical_test_df() |> factorize_columns()
    names(df) <- "cohort"
    obj <- crosstab_data(df)
    expect_equal(cohort_name(obj), "cohort_autogenerated")
})

test_that("crosstab_data() errors when cohort_col_name is provided, but df has some number of columns other than 2",{
    # 0 columns (fail)
    df <- data.frame() |> factorize_columns()
    expect_error(crosstab_data(df, "cohort"))

    # 1 column (pass)
    df <- get_categorical_test_df() |> factorize_columns()
    expect_error(crosstab_data(df, "cohort"))

    # 2 columns (fail)
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_no_error(crosstab_data(df, "cohort"))

    # 3 columns (fail)
    df[["test3"]] <- 3
    expect_error(crosstab_data(df))

    # 4 columns (fail)
    df[["test4"]] <- 4
    expect_error(crosstab_data(df))
})

test_that("crosstab_data() errors when cohort_col_name is not one of the columns in df",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns()
    expect_error(crosstab_data(df, "test"))
})

test_that("crosstab_data() errors when the cohort column is a list",{
    df <- get_multianswer_test_df(gr = T) |> factorize_columns()
    expect_error(crosstab_data(df, "variable"))
})

test_that("crosstab_data() warns when there is an empty column",{
    df <- get_categorical_test_df(gr = T)
    df[["variable"]] = NA
    df <- factorize_columns(df)
    expect_warning(crosstab_data(df, "cohort"))
})

test_that("crosstab_data() doesn't coerce numeric columns to factors",{
    df <- get_numeric_test_df(gr = T) |> factorize_columns("cohort")
    expect_no_warning(crosstab_data(df, "cohort"))
    obj <- suppressWarnings(crosstab_data(df, "cohort"))
    expect_true(is.numeric(obj[["variable"]]))
})

test_that("crosstab_data() coerces list columns to factor lists",{
    df <- get_multianswer_test_df(gr = T) |> factorize_columns("cohort")
    expect_warning(crosstab_data(df, "cohort"))
    obj <- suppressWarnings(crosstab_data(df, "cohort"))
    expect_true(is.factorlist(obj[["variable"]]))
})

test_that("crosstab_data() coerces non-numeric, non-list columns to factor",{
    df <- get_categorical_test_df(gr = T) |> factorize_columns("cohort")
    expect_warning(crosstab_data(df, "cohort"))
    obj <- suppressWarnings(crosstab_data(df, "cohort"))
    expect_true(is.factor(obj[["variable"]]))

    df <- get_likert_test_df(gr = T) |> factorize_columns("cohort")
    map <- default_likert_map(factor(df[["variable"]]))
    expect_warning(crosstab_data(df, "cohort", likert_map = map))
    obj <- suppressWarnings(crosstab_data(df, "cohort", likert_map = map))
    expect_true(is.factor(obj[["variable"]]))
})

test_that("crosstab_data() detects the right class for different variable types",{
    df <- get_categorical_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_s3_class(obj, "crosstab_data")

    df <- get_categorical_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_s3_class(obj, "crosstab_data_cat")

    df <- get_numeric_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_s3_class(obj, "crosstab_data")

    df <- get_numeric_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_s3_class(obj, "crosstab_data_num")

    df <- get_likert_test_df() |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    obj <- crosstab_data(df, likert_map = map)
    expect_s3_class(obj, "crosstab_data")

    df <- get_likert_test_df() |> factorize_columns()
    map <- default_likert_map(df[["variable"]])
    obj <- crosstab_data(df, likert_map = map)
    expect_s3_class(obj, "crosstab_data_likert")

    df <- get_multianswer_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_s3_class(obj, "crosstab_data")

    df <- get_multianswer_test_df() |> factorize_columns()
    obj <- crosstab_data(df)
    expect_s3_class(obj, "crosstab_data_multi")
})

# GETTERS ####
test_that("var_name() returns correct value", {
    expect_equal(var_name(test_ct), "var")
})

test_that("cohort_name() returns correct value", {
    expect_equal(cohort_name(test_ct), "cohort")
})

test_that("var_levels() returns correct value", {
    expect_equal(var_levels(test_ct), levels(test_df$var))
})

test_that("cohort_levels() returns correct value", {
    expect_equal(cohort_levels(test_ct), levels(test_df$cohort))
})

test_that("var() returns correct column", {
    expect_equal(var(test_ct), test_df$var)
})

test_that("cohort() returns correct column", {
    expect_equal(cohort(test_ct), test_df$cohort)
})

test_that("var_mapping() returns correct mapping", {
    df <- data.frame(
        var = factor(c("Low", "Medium", "High", "Medium"), levels = c("Low", "Medium", "High")),
        cohort = factor(c("A", "A", "B", "B"))
    )
    mapping <- c(Low = 1, Medium = 2, High = 3)
    ct <- new_crosstab_data_likert(df, "var", levels(df$var), mapping, "cohort", levels(df$cohort))

    expect_equal(var_mapping(ct), mapping)
})

test_that("var_mapped() maps values correctly", {
    df <- data.frame(
        var = factor(c("Low", "Medium", "High", "Medium"), levels = c("Low", "Medium", "High")),
        cohort = factor(c("A", "A", "B", "B"))
    )
    mapping <- c(Low = 1, Medium = 2, High = 3)
    ct <- new_crosstab_data_likert(df, "var", levels(df$var), mapping, "cohort", levels(df$cohort))

    expect_equal(var_mapped(ct), mapping[df$var])
})

test_that("is_grouped() returns accurate values",{
    df <- get_categorical_test_df(gr = T) |>
        factorize_columns()
    ct_data <- crosstab_data(df, "cohort")
    expect_true(is_grouped(ct_data))

    df <- get_categorical_test_df() |>
        factorize_columns()
    ct_data <- crosstab_data(df)
    expect_false(is_grouped(ct_data))
})

test_that("get_raw_data() returns the grouped data without the \"all\" values",{
    df <- get_categorical_test_df(gr = T) |>
        factorize_columns()
    ct_data <- crosstab_data(df, "cohort")
    expect_false("All" %in% cohort(get_raw_data(ct_data)))
})

# SETTERS ####
test_that("var_name<- updates column name and attribute", {
    ct <- test_ct
    var_name(ct) <- "variable"
    expect_equal(var_name(ct), "variable")
    expect_true("variable" %in% names(ct))
})

test_that("cohort_name<- updates column name and attribute", {
    ct <- test_ct
    cohort_name(ct) <- "group"
    expect_equal(cohort_name(ct), "group")
    expect_true("group" %in% names(ct))
})

test_that("var<- replaces column values correctly", {
    ct <- test_ct
    new_var <- factor(c("b", "a", "b", "c"), levels = c("a", "b", "c"))
    var(ct) <- new_var
    expect_equal(var(ct), new_var)
})

test_that("var<- updates var_levels", {
    ct <- test_ct
    new_var <- factor(c("b", "a", "b", "c"), levels = c("a", "b", "c"))
    var(ct) <- new_var
    expect_equal(var_levels(ct), levels(new_var))
})

test_that("var_levels<- updates levels of var() correctly", {
    ct <- test_ct
    new_levels <- c("c", "b", "a") # reordering
    var_levels(ct) <- new_levels
    expect_equal(levels(var(ct)), new_levels)
})

test_that("var_levels<- updates attribute correctly", {
    ct <- test_ct
    new_levels <- c("c", "b", "a")
    var_levels(ct) <- new_levels
    expect_equal(var_levels(ct), new_levels)
})

test_that("cohort<- replaces values correctly", {
    ct <- test_ct
    new_cohort <- factor(c("G2", "G1", "G1", "G2"))
    cohort(ct) <- new_cohort
    expect_equal(cohort(ct), new_cohort)
})

test_that("cohort<- updates cohort_levels correctly", {
    ct <- test_ct
    new_cohort <- factor(c("G2", "G1", "G1", "G2"))
    cohort(ct) <- new_cohort
    expect_equal(cohort_levels(ct), levels(new_cohort))
})

test_that("cohort_levels<- updates levels of cohort() correctly", {
    ct <- test_ct
    new_levels <- c("G2", "G1")
    cohort_levels(ct) <- new_levels
    expect_equal(levels(cohort(ct)), new_levels)
})

test_that("cohort_levels<- updates attribute correctly", {
    ct <- test_ct
    new_levels <- c("G2", "G1")
    cohort_levels(ct) <- new_levels
    expect_equal(cohort_levels(ct), new_levels)
})

test_that("var_mapping<- updates mapping correctly", {
    df <- data.frame(
        var = factor(c("Low", "Medium", "High", "Medium"), levels = c("Low", "Medium", "High")),
        cohort = factor(c("A", "A", "B", "B"))
    )
    mapping <- c(Low = 1, Medium = 2, High = 3)
    new_mapping <- c(Low = 0.5, Medium = 1.5, High = 2.5)
    ct <- new_crosstab_data_likert(df, "var", levels(df$var), mapping, "cohort", levels(df$cohort))

    var_mapping(ct) <- new_mapping
    expect_equal(var_mapping(ct), new_mapping)
})
