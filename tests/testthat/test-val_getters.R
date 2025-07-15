
# get_total()
test_that("get_total() works when provided proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_total(test_ct))
    result <- get_total(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    total_col <- result[["total"]]
    expect_equal(total_col[1], sum(total_col[2:length(total_col)]))
})

test_that("get_total() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_total(test_ct, out_col_name = "test_col"))
    result <- get_total(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

# get_complete()
test_that("get_complete() works when provided proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_complete(test_ct))
    result <- get_complete(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    total_col <- result[["complete"]]
    expect_equal(total_col[1], sum(total_col[2:length(total_col)]))
})

test_that("get_complete() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_complete(test_ct, out_col_name = "test_col"))
    result <- get_complete(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

# get_mean()
test_that("get_mean() works when provided proper data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_mean(test_ct))
    result <- get_mean(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    expect_true(is.numeric(result[["mean"]]))
})

test_that("get_mean() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_mean(test_ct, out_col_name = "test_col"))
    result <- get_mean(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_mean() respects round_to",{
    test_df <- num_test_df()
    test_df[["variable"]] <- test_df[["variable"]] / 113 * 100 # Make sure there are plenty of decimals
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_mean(test_ct, round_to = 0))
    result1 <- get_mean(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_mean(test_ct, round_to = 7))
    result2 <- get_mean(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))

    means1 <- result1[["mean"]] |> as.character()
    means2 <- result2[["mean"]] |> as.character()
    expect_true(all(means1 != means2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- means1[!is.na(means1)]
    expect_false(any(grepl("\\.", non_na_vals)))
})

# get_sd()
test_that("get_sd() works when provided proper data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_sd(test_ct))
    result <- get_sd(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    expect_true(is.numeric(result[["sd"]]))
})

test_that("get_sd() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_sd(test_ct, out_col_name = "test_col"))
    result <- get_sd(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_sd() respects round_to",{
    test_df <- num_test_df()
    test_df[["variable"]] <- test_df[["variable"]] / 113 * 100 # Make sure there are plenty of decimals
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_sd(test_ct, round_to = 0))
    result1 <- get_sd(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_sd(test_ct, round_to = 7))
    result2 <- get_sd(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))

    sds1 <- result1[["sd"]] |> as.character()
    sds2 <- result2[["sd"]] |> as.character()
    expect_true(all(sds1 != sds2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- sds1[!is.na(sds1)]
    expect_false(any(grepl("\\.", non_na_vals)))
})

# get_med()
test_that("get_med() works when provided proper data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_med(test_ct))
    result <- get_med(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    expect_true(is.numeric(result[["med"]]))
})

test_that("get_med() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_med(test_ct, out_col_name = "test_col"))
    result <- get_med(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_med() respects round_to",{
    test_df <- num_test_df()
    test_df[["variable"]] <- test_df[["variable"]] / 113 * 99 # Make sure there are plenty of decimals
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_med(test_ct, round_to = 0))
    result1 <- get_med(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_med(test_ct, round_to = 7))
    result2 <- get_med(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))

    meds1 <- result1[["med"]] |> as.character()
    meds2 <- result2[["med"]] |> as.character()

    expect_true(all(meds1 != meds2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- meds1[!is.na(meds1)]
    expect_false(any(grepl("\\.", non_na_vals)))
})

# get_q1()
test_that("get_q1() works when provided proper data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_q1(test_ct))
    result <- get_q1(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    expect_true(is.numeric(result[["q1"]]))
})

test_that("get_q1() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_q1(test_ct, out_col_name = "test_col"))
    result <- get_q1(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_q1() respects round_to",{
    test_df <- num_test_df()
    test_df[["variable"]] <- test_df[["variable"]] / 113 * 100 # Make sure there are plenty of decimals
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_q1(test_ct, round_to = 0))
    result1 <- get_q1(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_q1(test_ct, round_to = 7))
    result2 <- get_q1(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))

    q1s1 <- result1[["q1"]] |> as.character()
    q1s2 <- result2[["q1"]] |> as.character()
    expect_true(all(q1s1 != q1s2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- q1s1[!is.na(q1s1)]
    expect_false(any(grepl("\\.", non_na_vals)))
})

# get_q3()
test_that("get_q3() works when provided proper data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_q3(test_ct))
    result <- get_q3(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)

    expect_true(is.numeric(result[["q3"]]))
})

test_that("get_q3() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_q3(test_ct, out_col_name = "test_col"))
    result <- get_q3(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_q3() respects round_to",{
    test_df <- num_test_df()
    test_df[["variable"]] <- test_df[["variable"]] / 113 * 100 # Make sure there are plenty of decimals
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_q3(test_ct, round_to = 0))
    result1 <- get_q3(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_q3(test_ct, round_to = 7))
    result2 <- get_q3(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))

    q3s1 <- result1[["q3"]] |> as.character()
    q3s2 <- result2[["q3"]] |> as.character()
    expect_true(all(q3s1 != q3s2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- q3s1[!is.na(q3s1)]
    expect_false(any(grepl("\\.", non_na_vals)))
})

# get_count()
test_that("get_count() works when provided proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count(test_ct))
    result <- get_count(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 30)
    expect_true(is.numeric(result[["count"]]))
})

test_that("get_count() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count(test_ct, out_col_name = "test_col"))
    result <- get_count(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_in("test_col", names(result))
})

# get_percent()
test_that("get_percent() works when provided proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent(test_ct))
    result <- get_percent(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 30)
    expect_true(is.numeric(result[["percent"]]))
})

test_that("get_percent() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent(test_ct, out_col_name = "test_col"))
    result <- get_percent(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_in("test_col", names(result))
})

test_that("get_percent() respects round_to",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent(test_ct, round_to = 0))
    result1 <- get_percent(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 3)

    expect_silent(get_percent(test_ct, round_to = 7))
    result2 <- get_percent(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))
    expect_true(all(result1[["variable"]] == result2[["variable"]], na.rm = T))

    percents1 <- result1[["percent"]] |> as.character()
    percents2 <- result2[["percent"]] |> as.character()

    expect_false(all(percents1 == percents2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- percents1[!is.na(percents1)]
    expect_false(any(grepl("\\.", non_na_vals)))
})
