# is.crosstab() ####
test_that("is.crosstab() works correctly", {
    fake_ct <- structure(list(), class = CT_CLASS)
    fake_ct_data <- structure(list(), class = CT_DATA_CLASS)

    expect_true(is.crosstab(fake_ct))
    expect_false(is.crosstab(fake_ct_data))
    expect_true(is.crosstab(fake_ct_data, strict = FALSE))
    expect_false(is.crosstab(NULL))
})

test_that("is.crosstab.data works correctly", {
    fake_ct_data <- structure(list(), class = CT_DATA_CLASS)
    expect_true(is.crosstab.data(fake_ct_data))
    expect_false(is.crosstab.data(NULL))
    expect_false(is.crosstab.data(list()))
})

test_that("is.crosstab type functions work correctly", {
    make_obj <- function(class) structure(list(), class = class)

    # For each type, test with raw and wrapped versions
    expect_true(is.crosstab.categorical(make_obj(CT_DATA_CLASS_CAT)))
    expect_false(is.crosstab.categorical(make_obj("not_crosstab")))
    expect_false(is.crosstab.categorical(NULL))

    expect_true(is.crosstab.numeric(make_obj(CT_DATA_CLASS_NUM)))
    expect_false(is.crosstab.numeric(make_obj("not_crosstab")))
    expect_false(is.crosstab.numeric(NULL))

    expect_true(is.crosstab.likert(make_obj(CT_DATA_CLASS_LIKERT)))
    expect_false(is.crosstab.likert(make_obj("not_crosstab")))
    expect_false(is.crosstab.likert(NULL))

    expect_true(is.crosstab.multi(make_obj(CT_DATA_CLASS_MULTI)))
    expect_false(is.crosstab.multi(make_obj("not_crosstab")))
    expect_false(is.crosstab.multi(NULL))

    expect_true(is.crosstab.grouped(make_obj(CT_DATA_CLASS_GROUPED)))
    expect_false(is.crosstab.grouped(make_obj("not_crosstab")))
    expect_false(is.crosstab.grouped(NULL))
})

test_that("is.crosstab type functions recognize crosstab objects", {
    make_obj <- function(class) structure(list(), class = c(class, CT_DATA_CLASS))

    expect_true(is.crosstab.categorical(make_obj(CT_DATA_CLASS_CAT)))
    expect_true(is.crosstab.numeric(make_obj(CT_DATA_CLASS_NUM)))
    expect_true(is.crosstab.likert(make_obj(CT_DATA_CLASS_LIKERT)))
    expect_true(is.crosstab.multi(make_obj(CT_DATA_CLASS_MULTI)))
    expect_true(is.crosstab.grouped(make_obj(CT_DATA_CLASS_GROUPED)))
})

# assert_crosstab() ####
test_that("assert_* functions work and fail with correct messages", {
    make_obj <- function(class) structure(list(), class = class)

    expect_silent(assert_crosstab(make_obj(CT_CLASS)))
    expect_error(assert_crosstab(make_obj("wrong_class")))

    expect_silent(assert_crosstab_data(make_obj(CT_DATA_CLASS)))
    expect_error(assert_crosstab_data(make_obj("wrong_class")))

    expect_silent(assert_crosstab_categorical(make_obj(CT_DATA_CLASS_CAT)))
    expect_error(assert_crosstab_categorical(make_obj("wrong_class")))

    expect_silent(assert_crosstab_numeric(make_obj(CT_DATA_CLASS_NUM)))
    expect_error(assert_crosstab_numeric(make_obj("wrong_class")))

    expect_silent(assert_crosstab_likert(make_obj(CT_DATA_CLASS_LIKERT)))
    expect_error(assert_crosstab_likert(make_obj("wrong_class")))

    expect_silent(assert_crosstab_multi(make_obj(CT_DATA_CLASS_MULTI)))
    expect_error(assert_crosstab_multi(make_obj("wrong_class")))

    expect_silent(assert_crosstab_grouped(make_obj(CT_DATA_CLASS_GROUPED)))
    expect_error(assert_crosstab_grouped(make_obj("wrong_class")))
})

# as.crosstab.cat() ####
test_that("as.crosstab.cat() works when given categorical data",{
    ct <- crosstab(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct))

    expect_silent(as.crosstab.cat(ct))
    result <- as.crosstab.cat(ct)

    expect_true(is.crosstab.categorical(result))
})

test_that("as.crosstab.cat() works when given numerical data",{
    ct <- crosstab(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct))

    expect_silent(as.crosstab.cat(ct))
    result <- as.crosstab.cat(ct)

    expect_true(is.crosstab.categorical(result))
})

test_that("as.crosstab.cat() works when given likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])

    ct <- crosstab(lik_test_df(), "cohort", var_map = test_map)
    expect_true(is.crosstab.likert(ct))

    expect_silent(as.crosstab.cat(ct))
    result <- as.crosstab.cat(ct)

    expect_true(is.crosstab.categorical(result))
})

test_that("as.crosstab.cat() works when given multianswer data",{
    ct <- crosstab(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct))

    expect_silent(as.crosstab.cat(ct))
    result <- as.crosstab.cat(ct)

    assert_crosstab_categorical(result)
})

# as.crosstab.num() ####
test_that("as.crosstab.num() works when given categorical data",{
    ct <- crosstab(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct))

    expect_silent(as.crosstab.num(ct))
    result <- as.crosstab.num(ct)

    expect_true(is.crosstab.numeric(result))
})

test_that("as.crosstab.num() works when given numeric data",{
    ct <- crosstab(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct))

    expect_silent(as.crosstab.num(ct))
    result <- as.crosstab.num(ct)

    expect_true(is.crosstab.numeric(result))
})

test_that("as.crosstab.num() works when given likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])

    ct <- crosstab(lik_test_df(), "cohort", var_map = test_map)
    expect_true(is.crosstab.likert(ct))

    expect_silent(as.crosstab.num(ct))
    result <- as.crosstab.num(ct)

    expect_true(is.crosstab.numeric(result))
})

test_that("as.crosstab.num() works when given multianswer data",{
    ct <- crosstab(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct))

    expect_silent(as.crosstab.num(ct))
    result <- as.crosstab.num(ct)

    expect_true(is.crosstab.numeric(result))
})

# as.crosstab.likert() ####
test_that("as.crosstab.likert() works when given categorical data",{
    test_map <- 1:length(categorical_levels)
    names(test_map) <- categorical_levels

    ct <- crosstab(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct))

    expect_silent(as.crosstab.likert(ct, var_map = test_map))
    result <- as.crosstab.likert(ct, var_map = test_map)

    expect_true(is.crosstab.likert(result))
})

test_that("as.crosstab.likert() works when given numeric data",{
    test_map <- 1:length(categorical_levels)
    names(test_map) <- categorical_levels

    test_df <- num_test_df()
    test_df[["variable"]] <- (test_df[["variable"]] %% length(categorical_levels)) + 1
    ct <- crosstab(test_df, "cohort")
    expect_true(is.crosstab.numeric(ct))

    expect_silent(as.crosstab.likert(ct, var_map = test_map))
    result <- as.crosstab.likert(ct, var_map = test_map)

    expect_true(is.crosstab.likert(result))
})

test_that("as.crosstab.likert() works when given likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])

    test_map_2 <- test_map
    names(test_map_2) <- rev(names(test_map_2))

    ct <- crosstab(lik_test_df(), "cohort", var_map = test_map_2)
    expect_true(is.crosstab.likert(ct))

    expect_silent(as.crosstab.likert(ct, var_map = test_map))
    result <- as.crosstab.likert(ct, var_map = test_map)

    expect_true(is.crosstab.likert(result))
})

test_that("as.crosstab.likert() works when given multianswer data",{
    test_map <- 1:length(multianswer_levels)
    names(test_map) <- multianswer_levels

    ct <- crosstab(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct))

    expect_silent(as.crosstab.likert(ct, var_map = test_map))
    result <- as.crosstab.likert(ct, var_map = test_map)

    expect_true(is.crosstab.likert(result))
})

# as.crosstab.multi() ####
test_that("as.crosstab.multi() works when given categorical data",{
    ct <- crosstab(cat_test_df(), "cohort")
    expect_true(is.crosstab.categorical(ct))

    expect_silent(as.crosstab.multi(ct))
    result <- as.crosstab.multi(ct)

    expect_true(is.crosstab.multi(result))
})

test_that("as.crosstab.multi() works when given numeric data",{
    ct <- crosstab(num_test_df(), "cohort")
    expect_true(is.crosstab.numeric(ct))

    expect_silent(as.crosstab.multi(ct))
    result <- as.crosstab.multi(ct)

    expect_true(is.crosstab.multi(result))
})

test_that("as.crosstab.multi() works when given likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])

    ct <- crosstab(lik_test_df(), "cohort", var_map = test_map)
    expect_true(is.crosstab.likert(ct))

    expect_silent(as.crosstab.multi(ct))
    result <- as.crosstab.multi(ct)

    expect_true(is.crosstab.multi(result))
})

test_that("as.crosstab.multi() works when given multianswer data",{
    ct <- crosstab(multi_test_df(), "cohort")
    expect_true(is.crosstab.multi(ct))

    expect_silent(as.crosstab.multi(ct))
    result <- as.crosstab.multi(ct)

    expect_true(is.crosstab.multi(result))
})
