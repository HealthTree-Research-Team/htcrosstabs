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
