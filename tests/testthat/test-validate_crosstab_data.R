# validate_input_new_crosstab_data() ####
test_that("validate_input_new_crosstab_data() works when given proper data",{
    expect_silent(validate_input_new_crosstab_data(
        df = cat_test_df(),
        var_col_name = "test_var",
        var_levels = categorical_levels,
        cohort_col_name = "test_cohort",
        cohort_levels = character_levels,
        grouped = TRUE,
        combined_cohort_name = "test_combined",
        desc_col_name = "test_description",
        subclass = CT_DATA_CLASS_CAT,
        var_map = NULL
    ))

    expect_silent(validate_input_new_crosstab_data(
        df = num_test_df(),
        var_col_name = "test_var",
        var_levels = NULL,
        cohort_col_name = "test_cohort",
        cohort_levels = character_levels,
        grouped = TRUE,
        combined_cohort_name = "test_combined",
        desc_col_name = "test_description",
        subclass = CT_DATA_CLASS_NUM,
        var_map = NULL
    ))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    expect_silent(validate_input_new_crosstab_data(
        df = test_df,
        var_col_name = "test_var",
        var_levels = likert_levels,
        cohort_col_name = "test_cohort",
        cohort_levels = character_levels,
        grouped = TRUE,
        combined_cohort_name = "test_combined",
        desc_col_name = "test_description",
        subclass = CT_DATA_CLASS_LIKERT,
        var_map = test_map
    ))

    expect_silent(validate_input_new_crosstab_data(
        df = multi_test_df(),
        var_col_name = "test_var",
        var_levels = multianswer_levels,
        cohort_col_name = "test_cohort",
        cohort_levels = character_levels,
        grouped = TRUE,
        combined_cohort_name = "test_combined",
        desc_col_name = "test_description",
        subclass = CT_DATA_CLASS_MULTI,
        var_map = NULL
    ))

    expect_silent(validate_input_new_crosstab_data(
        df = data.frame(),
        var_col_name = "test_var",
        var_levels = NULL,
        cohort_col_name = "test_cohort",
        cohort_levels = character_levels,
        grouped = FALSE,
        combined_cohort_name = "test_combined",
        desc_col_name = "test_description",
        subclass = NULL,
        var_map = NULL
    ))
})

test_that("validate_input_new_crosstab_data() fails if df is not a data.frame", {
    expect_error(
        validate_input_new_crosstab_data(
            df = "not a df",
            var_col_name = "var",
            cohort_col_name = "cohort",
            cohort_levels = c("X", "Y"),
            var_levels = NULL,
            var_map = NULL,
            subclass = NULL,
            grouped = TRUE,
            combined_cohort_name = "All",
            desc_col_name = "description"
        )
    )
})

test_that("validate_input_new_crosstab_data() fails if var_col_name is not a single character", {
    expect_error(
        validate_input_new_crosstab_data(
            df = data.frame(),
            var_col_name = c("var1", "var2"),
            cohort_col_name = "cohort",
            cohort_levels = c("X", "Y"),
            var_levels = NULL,
            var_map = NULL,
            subclass = NULL,
            grouped = TRUE,
            combined_cohort_name = "All",
            desc_col_name = "description"
        )
    )
})

test_that("validate_input_new_crosstab_data() fails if cohort_col_name is not a single character", {
    expect_error(
        validate_input_new_crosstab_data(
            df = data.frame(),
            var_col_name = "var",
            cohort_col_name = 123,
            cohort_levels = c("X", "Y"),
            var_levels = NULL,
            var_map = NULL,
            subclass = NULL,
            grouped = TRUE,
            combined_cohort_name = "All",
            desc_col_name = "description"
        )
    )
})

test_that("validate_input_new_crosstab_data() fails if cohort_levels is not character", {
    expect_error(
        validate_input_new_crosstab_data(
            df = data.frame(),
            var_col_name = "var",
            cohort_col_name = "cohort",
            cohort_levels = 1:2,
            var_levels = NULL,
            var_map = NULL,
            subclass = NULL,
            grouped = TRUE,
            combined_cohort_name = "All",
            desc_col_name = "description"
        )
    )
})

test_that("validate_input_new_crosstab_data() fails if grouped is not logical", {
    expect_error(
        validate_input_new_crosstab_data(
            df = data.frame(),
            var_col_name = "var",
            cohort_col_name = "cohort",
            cohort_levels = c("A", "B"),
            var_levels = NULL,
            var_map = NULL,
            subclass = NULL,
            grouped = "yes",
            combined_cohort_name = "All",
            desc_col_name = "description"
        )
    )
})

test_that("validate_input_new_crosstab_data() fails if var_map is not numeric or not named", {
    expect_error(
        validate_input_new_crosstab_data(
            df = data.frame(),
            var_col_name = "var",
            cohort_col_name = "cohort",
            cohort_levels = c("X", "Y"),
            var_levels = NULL,
            var_map = c(1, 2),  # unnamed
            subclass = NULL,
            grouped = TRUE,
            combined_cohort_name = "All",
            desc_col_name = "description"
        )
    )
})

test_that("validate_input_new_crosstab_data() accepts NULL for optional inputs", {
    expect_silent(
        validate_input_new_crosstab_data(
            df = data.frame(var = c("A", "B")),
            var_col_name = "var",
            cohort_col_name = "cohort",
            cohort_levels = c("X", "Y"),
            var_levels = NULL,
            var_map = NULL,
            subclass = NULL,
            grouped = FALSE,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

# validate_input_crosstab_data() ####
test_that("validate_input_crosstab_data() accepts valid input (cohort_col_name present)", {
    df <- data.frame(var = c("A", "B"), cohort = c("G1", "G2"))
    var_map <- c(A = 1, B = 2)

    expect_silent(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = "cohort",
            var_map = var_map,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() accepts valid input (cohort_col_name NULL)", {
    df <- data.frame(var = c("A", "B"))
    expect_silent(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = NULL,
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if df is not a data.frame", {
    expect_error(
        validate_input_crosstab_data(
            df = "not a df",
            cohort_col_name = NULL,
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if cohort_col_name is not character", {
    df <- data.frame(var = c("A", "B"), cohort = c("G1", "G2"))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = 123,
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if var_map is not numeric or not named", {
    df <- data.frame(var = c("A", "B"))
    bad_map <- c(1, 2) # unnamed

    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = NULL,
            var_map = bad_map,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if combined_cohort_name is not character", {
    df <- data.frame(var = c("A", "B"))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = NULL,
            var_map = NULL,
            combined_cohort_name = 42,
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if desc_col_name is not character", {
    df <- data.frame(var = c("A", "B"))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = NULL,
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = 42
        )
    )
})

test_that("validate_input_crosstab_data() warns if any columns are entirely NA", {
    df <- data.frame(var = c("A", "B"), empty_col = NA)
    expect_warning(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = "empty_col",
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if cohort_col_name is specified but df doesn't have 2 columns", {
    df <- data.frame(var = c("A", "B"))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = "cohort",
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if cohort_col_name not found in df", {
    df <- data.frame(var = c("A", "B"), not_cohort = c("x", "y"))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = "cohort",
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if cohort_col_name column is a list", {
    df <- data.frame(var = c("A", "B"), cohort = I(list(1, 2)))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = "cohort",
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if combined_cohort_name already in cohort column", {
    df <- data.frame(var = c("A", "B"), cohort = c("All", "G2"))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = "cohort",
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

test_that("validate_input_crosstab_data() fails if cohort_col_name is NULL but df has more than 1 column", {
    df <- data.frame(var = c("A", "B"), extra = c(1, 2))
    expect_error(
        validate_input_crosstab_data(
            df = df,
            cohort_col_name = NULL,
            var_map = NULL,
            combined_cohort_name = "All",
            desc_col_name = "desc"
        )
    )
})

# validate_crosstab_data.crosstab_data() ####
test_that("validate_crosstab_data.crosstab_data() works when given proper data",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- cat_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- num_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, var_map = test_map)
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))

    test_df <- multi_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if ct_data isn't a crosstab_data object",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data(test_ct))
    expect_error(validate_crosstab_data.crosstab_data(data.frame()))
    expect_error(validate_crosstab_data.crosstab_data(1))
    expect_error(validate_crosstab_data.crosstab_data("a"))
})

test_that("validate_crosstab_data.crosstab_data() fails if ct_data doesn't have 2 columns",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    malformed_ct_data <- ct_data[, 1, drop = F]
    expect_error(validate_crosstab_data.crosstab_data(malformed_ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if ct_data doesn't have var_col_name",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    malformed_ct_data <- ct_data
    attr(malformed_ct_data, "var_col_name") <- NULL
    expect_error(validate_crosstab_data.crosstab_data(malformed_ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if column names don't include var_col_name",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    malformed_ct_data <- ct_data
    attr(malformed_ct_data, "var_col_name") <- "not in the data frame"
    expect_error(validate_crosstab_data.crosstab_data(malformed_ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if cohort_col_name is missing", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "cohort_col_name") <- NULL
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if cohort_col_name not in column names", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "cohort_col_name") <- "missing_col"
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if combined_cohort_name is not character", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "combined_cohort_name") <- 123  # not character
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if cohort column is a list", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    ct_data$cohort <- sapply(ct_data$cohort, function(x) I(list(x)))
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})


test_that("validate_crosstab_data.crosstab_data() fails if cohort column is not a factor", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    ct_data$cohort <- as.character(ct_data$cohort)  # make it character
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if cohort_levels attribute is missing", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "cohort_levels") <- NULL
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if cohort values not in cohort_levels", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "cohort_levels") <- c("a", "b")  # force mismatch
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if desc_col_name is missing", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "desc_col_name") <- NULL
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

test_that("validate_crosstab_data.crosstab_data() fails if desc_col_name matches other column names", {
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "desc_col_name") <- attr(ct_data, "var_col_name")  # duplicate with var_col_name
    expect_error(validate_crosstab_data.crosstab_data(ct_data))

    attr(ct_data, "desc_col_name") <- attr(ct_data, "cohort_col_name")  # duplicate with cohort_col_name
    expect_error(validate_crosstab_data.crosstab_data(ct_data))
})

# validate_crosstab_data.crosstab_data_cat() ####
test_that("validate_crosstab_data.crosstab_data_cat() works when given proper data",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data.crosstab_data_cat(ct_data))

    test_df <- cat_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data.crosstab_data_cat(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails when given a non-categorical crosstab object",{
    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))

    test_df <- num_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, var_map = test_map)
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))

    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))

    test_df <- multi_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))

    expect_error(validate_crosstab_data.crosstab_data_cat(NULL))
    expect_error(validate_crosstab_data.crosstab_data_cat(TRUE))
    expect_error(validate_crosstab_data.crosstab_data_cat(1))
    expect_error(validate_crosstab_data.crosstab_data_cat(c(1, 2, 3)))
    expect_error(validate_crosstab_data.crosstab_data_cat("a"))
    expect_error(validate_crosstab_data.crosstab_data_cat(c("a", "b", "c")))
    expect_error(validate_crosstab_data.crosstab_data_cat(list()))
    expect_error(validate_crosstab_data.crosstab_data_cat(data.frame()))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails when the data isn't a factor",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    ct_data[["variable"]] <- as.character(ct_data[["variable"]])
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails when var_levels attribute is missing",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "var_levels") <- NULL
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_cat() fails when var_levels doesn't cover all values in variable",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    old_levels <- attr(ct_data, "var_levels")
    attr(ct_data, "var_levels") <- old_levels[1:(length(old_levels)-1)]
    expect_error(validate_crosstab_data.crosstab_data_cat(ct_data))
})

# validate_crosstab_data.crosstab_data_num() ####
test_that("validate_crosstab_data.crosstab_data_num() works when given proper data",{
    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data.crosstab_data_num(ct_data))

    test_df <- num_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data.crosstab_data_num(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_num() fails when given a non-numeric crosstab object",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))

    test_df <- cat_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, var_map = test_map)
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))

    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))

    test_df <- multi_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))

    expect_error(validate_crosstab_data.crosstab_data_num(NULL))
    expect_error(validate_crosstab_data.crosstab_data_num(TRUE))
    expect_error(validate_crosstab_data.crosstab_data_num(1))
    expect_error(validate_crosstab_data.crosstab_data_num(c(1, 2, 3)))
    expect_error(validate_crosstab_data.crosstab_data_num("a"))
    expect_error(validate_crosstab_data.crosstab_data_num(c("a", "b", "c")))
    expect_error(validate_crosstab_data.crosstab_data_num(list()))
    expect_error(validate_crosstab_data.crosstab_data_num(data.frame()))
})

test_that("validate_crosstab_data.crosstab_data_num() fails when the data isn't numeric",{
    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    ct_data[["variable"]] <- as.character(ct_data[["variable"]])
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_num() fails when var_levels attribute is present",{
    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "var_levels") <- c("1", "2", "3")
    expect_error(validate_crosstab_data.crosstab_data_num(ct_data))
})

# validate_crosstab_data.crosstab_data_likert() ####
test_that("validate_crosstab_data.crosstab_data_likert() works when given proper data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_silent(validate_crosstab_data.crosstab_data_likert(ct_data))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, var_map = test_map)
    expect_silent(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when given a non-likert crosstab object",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    test_df <- cat_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    test_df <- num_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    test_df <- multi_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    expect_error(validate_crosstab_data.crosstab_data_likert(NULL))
    expect_error(validate_crosstab_data.crosstab_data_likert(TRUE))
    expect_error(validate_crosstab_data.crosstab_data_likert(1))
    expect_error(validate_crosstab_data.crosstab_data_likert(c(1, 2, 3)))
    expect_error(validate_crosstab_data.crosstab_data_likert("a"))
    expect_error(validate_crosstab_data.crosstab_data_likert(c("a", "b", "c")))
    expect_error(validate_crosstab_data.crosstab_data_likert(list()))
    expect_error(validate_crosstab_data.crosstab_data_likert(data.frame()))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when the data isn't a factor",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    ct_data[["variable"]] <- as.character(ct_data[["variable"]])
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when var_levels attribute is missing",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    attr(ct_data, "var_levels") <- NULL
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when var_levels attribute doesn't cover all values in variable column",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    old_levels <- attr(ct_data, "var_levels")
    attr(ct_data, "var_levels") <- old_levels[1:(length(old_levels)-1)]
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when var_map attribute is missing",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    attr(ct_data, "var_map") <- NULL
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when var_map is not a named numeric vector",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- test_map
    names(new_map) <- NULL
    attr(ct_data, "var_map") <- new_map
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))

    new_map <- as.character(test_map)
    names(new_map) <- names(test_map)
    attr(ct_data, "var_map") <- new_map
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when var_map has duplicate names",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- test_map
    new_map <- c(new_map, new_map[2])
    new_map[length(new_map)] <- 100
    attr(ct_data, "var_map") <- new_map
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_likert() fails when var_map names don't map all values in variable",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- test_map[1:(length(test_map)-1)]
    attr(ct_data, "var_map") <- new_map
    expect_error(validate_crosstab_data.crosstab_data_likert(ct_data))
})

# validate_crosstab_data.crosstab_data_multi() ####
test_that("validate_crosstab_data.crosstab_data_multi() works when given proper data",{
    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data.crosstab_data_multi(ct_data))

    test_df <- multi_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data.crosstab_data_multi(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails when given a non-categorical crosstab object",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))

    test_df <- cat_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))

    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))

    test_df <- num_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, var_map = test_map)
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))

    expect_error(validate_crosstab_data.crosstab_data_multi(NULL))
    expect_error(validate_crosstab_data.crosstab_data_multi(TRUE))
    expect_error(validate_crosstab_data.crosstab_data_multi(1))
    expect_error(validate_crosstab_data.crosstab_data_multi(c(1, 2, 3)))
    expect_error(validate_crosstab_data.crosstab_data_multi("a"))
    expect_error(validate_crosstab_data.crosstab_data_multi(c("a", "b", "c")))
    expect_error(validate_crosstab_data.crosstab_data_multi(list()))
    expect_error(validate_crosstab_data.crosstab_data_multi(data.frame()))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails when the data isn't a factorlist",{
    test_df_raw <- multi_test_df(factorize = F)
    test_df <- test_df_raw |> factorize_columns()
    ct_data <- crosstab_data(test_df, "cohort")
    ct_data[["variable"]] <- test_df_raw[["variable"]]
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails when var_levels attribute is missing",{
    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    attr(ct_data, "var_levels") <- NULL
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))
})

test_that("validate_crosstab_data.crosstab_data_multi() fails when var_levels doesn't cover all values in variable",{
    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    old_levels <- attr(ct_data, "var_levels")
    attr(ct_data, "var_levels") <- old_levels[1:(length(old_levels)-1)]
    expect_error(validate_crosstab_data.crosstab_data_multi(ct_data))
})

# validate_crosstab_data() ####
test_that("validate_crosstab_data() accurately calls the appropriate function",{
    test_df <- cat_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- cat_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- num_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- num_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- lik_test_df(gr = F)
    test_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, var_map = test_map)
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_silent(validate_crosstab_data(ct_data))

    test_df <- multi_test_df(gr = F)
    ct_data <- crosstab_data(test_df)
    expect_silent(validate_crosstab_data(ct_data))
})

test_that("validate_crosstab_data() fails when called on a non-crosstab data object",{
    expect_error(validate_crosstab_data(NULL))
    expect_error(validate_crosstab_data(TRUE))
    expect_error(validate_crosstab_data(1))
    expect_error(validate_crosstab_data(c(1, 2, 3)))
    expect_error(validate_crosstab_data("a"))
    expect_error(validate_crosstab_data(c("a", "b", "c")))
    expect_error(validate_crosstab_data(list()))
    expect_error(validate_crosstab_data(data.frame()))
})

# validate_input_var_levels_getter() ####
test_that("validate_input_var_levels_getter() works when given proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    expect_silent(validate_input_var_levels_getter(test_ct))

    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab_data(test_df, "cohort", var_map = test_map)
    expect_silent(validate_input_var_levels_getter(test_ct))

    test_df <- multi_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    expect_silent(validate_input_var_levels_getter(test_ct))
})

test_that("validate_input_var_levels_getter() fails when given numeric data",{
    test_df <- num_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    expect_error(validate_input_var_levels_getter(test_ct))
})

# validate_input_var_setter() ####
test_that("validate_input_var_setter() works when given proper data",{
    test_df1 <- cat_test_df()
    test_ct <- crosstab_data(test_df1, "cohort")
    test_df2 <- cat_test_df()
    expect_silent(validate_input_var_setter(test_ct, test_df2[["variable"]]))
})

test_that("validate_input_var_setter() fails when the data is of a different type",{
    test_df1 <- cat_test_df()
    test_ct <- crosstab_data(test_df1, "cohort")
    test_df2 <- num_test_df()
    expect_error(validate_input_var_setter(test_ct, test_df2[["variable"]]))
})

# validate_input_var_levels_setter() ####
test_that("validate_input_var_levels_setter() works when given proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- rev(var_levels(test_ct))
    expect_silent(validate_input_var_levels_setter(test_ct, new_levels))
})

test_that("validate_input_var_levels_setter() fails when given non-character values",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- c(1, 2, 3)
    expect_error(validate_input_var_levels_setter(test_ct, new_levels))
})

test_that("validate_input_var_levels_setter() fails when given numeric crosstab data",{
    test_df <- num_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- c("a", "b", "c")
    expect_error(validate_input_var_levels_setter(test_ct, new_levels))
})

test_that("validate_input_var_levels_setter() fails when levels have duplicates",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- c(var_levels(test_ct), var_levels(test_ct))
    expect_error(validate_input_var_levels_setter(test_ct, new_levels))
})

test_that("validate_input_var_levels_setter() fails when levels don't have the same values as the previous levels",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")

    new_levels <- var_levels(test_ct)[2:length(var_levels(test_ct))]
    expect_error(validate_input_var_levels_setter(test_ct, new_levels))

    new_levels <- c("a", "b", "c")
    expect_error(validate_input_var_levels_setter(test_ct, new_levels))
})

# validate_input_cohort_setter() ####
test_that("validate_input_cohort_setter() works when given proper data",{
    test_df1 <- cat_test_df(group_type = "n")
    test_ct <- crosstab_data(test_df1, "cohort")
    test_df2 <- cat_test_df(group_type = "c")
    expect_silent(validate_input_cohort_setter(test_ct, test_df2[["cohort"]]))
})

test_that("validate_input_cohort_setter() fails when cohort is not a factor",{
    test_df1 <- cat_test_df(group_type = "n")
    test_ct <- crosstab_data(test_df1, "cohort")
    test_df2 <- cat_test_df(group_type = "c", factorize = F)
    expect_error(validate_input_cohort_setter(test_ct, test_df2[["cohort"]]))
})

# validate_input_cohort_levels_setter() ####
test_that("validate_input_cohort_levels_setter() works when given proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- rev(cohort_levels(test_ct))
    expect_silent(validate_input_cohort_levels_setter(test_ct, new_levels))
})

test_that("validate_input_cohort_levels_setter() fails when given non-character values",{
    test_df <- num_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- c(1, 2, 3)
    expect_error(validate_input_cohort_levels_setter(test_ct, new_levels))
})

test_that("validate_input_cohort_levels_setter() fails when levels have duplicates",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")
    new_levels <- c(cohort_levels(test_ct), cohort_levels(test_ct))
    expect_error(validate_input_cohort_levels_setter(test_ct, new_levels))
})

test_that("validate_input_cohort_levels_setter() fails when levels don't have the same values as previous levels",{
    test_df <- cat_test_df()
    test_ct <- crosstab_data(test_df, "cohort")

    new_levels <- cohort_levels(test_ct)[2:length(cohort_levels(test_ct))]
    expect_error(validate_input_cohort_levels_setter(test_ct, new_levels))

    new_levels <- c("a", "b", "c")
    expect_error(validate_input_cohort_levels_setter(test_ct, new_levels))
})

# validate_input_var_map_setter() ####
test_that("validate_input_var_map_setter() works when given proper data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- test_map
    names(new_map) <- rev(names(new_map))
    expect_silent(validate_input_var_map_setter(test_ct, new_map))
})

test_that("validate_input_var_map_setter() fails when var_map isn't a named numeric vector",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- test_map
    names(new_map) <- NULL
    expect_error(validate_input_var_map_setter(test_ct, new_map))

    new_map <- as.character(test_map)
    names(new_map) <- names(test_map)
    expect_error(validate_input_var_map_setter(test_ct, new_map))
})

test_that("validate_input_var_map_setter() fails when var_map has duplicate names",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- c(test_map, test_map)
    expect_error(validate_input_var_map_setter(test_ct, new_map))
})

test_that("validate_input_var_map_setter() fails when var_map has duplicate names",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab_data(test_df, "cohort", var_map = test_map)

    new_map <- 2 * test_map
    names(new_map)[1] <- "new name that isn't in there"
    expect_error(validate_input_var_map_setter(test_ct, new_map))
})
