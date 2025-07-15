# categorical -> likert ####
test_that("validate_input_as_likert() works on categorical data",{
    new_map <- 1:length(categorical_levels)
    names(new_map) <- categorical_levels
    ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct_data))
    expect_silent(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on categorical data without var_map",{
    ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on categorical data when var_map isn't the proper type",{
    new_map <- c(a = "a", b = "b", c = "c")
    ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))

    new_map <- c(1, 2, 3)
    ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on categorical data when var_map doesn't map all values",{
    temp_levels <- categorical_levels[1:(length(categorical_levels)-1)]
    new_map <- 1:length(temp_levels)
    names(new_map) <- temp_levels
    ct_data <- crosstab_data(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

# numeric -> likert ####
test_that("validate_input_as_likert() works on numeric data",{
    new_map <- 1:length(categorical_levels)
    names(new_map) <- categorical_levels
    test_df <- num_test_df()
    test_df[["variable"]] <- (test_df[["variable"]] %% length(categorical_levels)) + 1
    ct_data <- crosstab_data(test_df, "cohort")
    expect_true(is.crosstab.numeric(ct_data))
    expect_silent(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on numeric data without var_map",{
    ct_data <- crosstab_data(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct_data))
    expect_error(validate_input_as_likert(ct_data, NULL))
})

test_that("validate_input_as_likert() errors on numeric data when var_map isn't the proper type",{
    new_map <- c(a = "a", b = "b", c = "c")
    ct_data <- crosstab_data(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))

    new_map <- c(1, 2, 3)
    ct_data <- crosstab_data(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on numeric data when var_map doesn't map all values",{
    temp_levels <- categorical_levels[1:(length(categorical_levels)-1)]
    new_map <- 1:length(temp_levels)
    names(new_map) <- temp_levels
    ct_data <- crosstab_data(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

# likert -> likert ####
test_that("validate_input_as_likert() works on likert data, even without var_map",{
    test_df <- lik_test_df()
    orig_map <- default_var_map(test_df[["variable"]])
    new_map <- orig_map
    names(new_map) <- rev(names(new_map))

    ct_data <- crosstab_data(test_df, "cohort", var_map = orig_map)
    expect_true(is.crosstab.likert(ct_data))
    expect_silent(validate_input_as_likert(ct_data, NULL))
    expect_silent(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on likert data when var_map isn't the proper type",{
    test_df <- lik_test_df()
    orig_map <- default_var_map(test_df[["variable"]])

    new_map <- c(a = "a", b = "b", c = "c")
    ct_data <- crosstab_data(test_df, "cohort", var_map = orig_map)
    expect_true(is.crosstab.likert(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))

    new_map <- c(1, 2, 3)
    ct_data <- crosstab_data(test_df, "cohort", var_map = orig_map)
    expect_true(is.crosstab.likert(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on likert data when var_map doesn't map all values",{
    test_df <- lik_test_df()
    orig_map <- default_var_map(test_df[["variable"]])
    temp_levels <- likert_levels[1:(length(likert_levels)-1)]
    new_map <- 1:length(temp_levels)
    names(new_map) <- temp_levels

    ct_data <- crosstab_data(test_df, "cohort", var_map = orig_map)
    expect_true(is.crosstab.likert(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

# multianswer -> likert ####
test_that("validate_input_as_likert() works on multianswer data",{
    test_df <- multi_test_df()
    new_map <- default_var_map(test_df[["variable"]])
    ct_data <- crosstab_data(test_df, "cohort")
    expect_true(is.crosstab.multi(ct_data))
    expect_silent(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on multianswer data without var_map",{
    test_df <- multi_test_df()
    ct_data <- crosstab_data(test_df, "cohort")
    expect_true(is.crosstab.multi(ct_data))
    expect_error(validate_input_as_likert(ct_data, NULL))
})

test_that("validate_input_as_likert() errors on multianswer data when var_map is the wrong type",{
    new_map <- c(a = "a", b = "b", c = "c")
    ct_data <- crosstab_data(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))

    new_map <- c(1, 2, 3)
    ct_data <- crosstab_data(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})

test_that("validate_input_as_likert() errors on multianswer data when not all values are mapped in var_map",{
    temp_levels <- multianswer_levels[1:(length(multianswer_levels)-1)]
    new_map <- 1:length(temp_levels)
    names(new_map) <- temp_levels
    ct_data <- crosstab_data(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct_data))
    expect_error(validate_input_as_likert(ct_data, new_map))
})
