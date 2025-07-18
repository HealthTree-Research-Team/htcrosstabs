# get_total() ####
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

# get_complete() ####
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

# get_complete_total() ####
test_that("get_complete_total() works when provided proper data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_complete_total(test_ct))
    result <- get_complete_total(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)
})

test_that("get_complete_total() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_complete_total(test_ct, out_col_name = "test_col"))
    result <- get_complete_total(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

# get_mean() ####
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

    expect_silent(get_mean(test_ct, round_to = NULL))
    result3 <- get_mean(test_ct, round_to = NULL)
    expect_false(all(result1[["mean"]] == result3[["mean"]]))
})

# get_sd() ####
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

    expect_silent(get_sd(test_ct, round_to = NULL))
    result3 <- get_sd(test_ct, round_to = NULL)
    expect_false(all(result1[["sd"]] == result3[["sd"]]))
})

# get_mean_sd() ####
test_that("get_mean_sd() works when provided numeric data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_mean_sd(test_ct))
    result <- get_mean_sd(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)
    expect_true(is.character(result[["mean_sd"]]))
})

test_that("get_mean_sd() works when provided likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", test_map)

    expect_silent(get_mean_sd(test_ct))
    result <- get_mean_sd(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)
    expect_true(is.character(result[["mean_sd"]]))
})

test_that("get_mean_sd() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_mean_sd(test_ct, out_col_name = "test_col"))
    result <- get_mean_sd(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_mean_sd() respects round_to",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_mean_sd(test_ct, round_to = 0))
    result1 <- get_mean_sd(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_mean_sd(test_ct, round_to = 7))
    result2 <- get_mean_sd(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))
    expect_true(all(result1[["variable"]] == result2[["variable"]], na.rm = T))

    mean_sd_1 <- result1[["mean_sd"]]
    mean_sd_2 <- result2[["mean_sd"]]

    expect_false(all(mean_sd_1 == mean_sd_2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- mean_sd_1[!is.na(mean_sd_1)]
    expect_false(any(grepl("\\.", non_na_vals)))

    expect_silent(get_mean_sd(test_ct, round_to = NULL))
    result3 <- get_mean_sd(test_ct, round_to = NULL)
    expect_false(all(result1[["mean_sd"]] == result3[["mean_sd"]]))
})

# get_med() ####
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

    expect_silent(get_med(test_ct, round_to = NULL))
    result3 <- get_med(test_ct, round_to = NULL)
    expect_false(all(result1[["med"]] == result3[["med"]]))
})

# get_q1() ####
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

    expect_silent(get_q1(test_ct, round_to = NULL))
    result3 <- get_q1(test_ct, round_to = NULL)
    expect_false(all(result1[["q1"]] == result3[["q1"]]))
})

# get_q3() ####
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

    expect_silent(get_q3(test_ct, round_to = NULL))
    result3 <- get_q3(test_ct, round_to = NULL)
    expect_false(all(result1[["q3"]] == result3[["q3"]]))
})

# get_med_iqr() ####
test_that("get_med_iqr() works when provided numeric data",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_med_iqr(test_ct))
    result <- get_med_iqr(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)
    expect_true(is.character(result[["med_iqr"]]))
})

test_that("get_med_iqr() works when provided likert data",{
    test_df <- lik_test_df()
    test_map <- default_var_map(test_df[["variable"]])
    test_ct <- crosstab(test_df, "cohort", test_map)

    expect_silent(get_med_iqr(test_ct))
    result <- get_med_iqr(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_equal(nrow(result), 6)
    expect_true(is.character(result[["med_iqr"]]))
})

test_that("get_med_iqr() respects out_col_name",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_med_iqr(test_ct, out_col_name = "test_col"))
    result <- get_med_iqr(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 2)
    expect_in("test_col", names(result))
})

test_that("get_med_iqr() respects round_to",{
    test_df <- num_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_med_iqr(test_ct, round_to = 0))
    result1 <- get_med_iqr(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 2)

    expect_silent(get_med_iqr(test_ct, round_to = 7))
    result2 <- get_med_iqr(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))
    expect_true(all(result1[["variable"]] == result2[["variable"]], na.rm = T))

    med_iqr_1 <- result1[["med_iqr"]]
    med_iqr_2 <- result2[["med_iqr"]]

    expect_false(all(med_iqr_1 == med_iqr_2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- med_iqr_1[!is.na(med_iqr_1)]
    expect_false(any(grepl("\\.", non_na_vals)))

    expect_silent(get_med_iqr(test_ct, round_to = NULL))
    result3 <- get_med_iqr(test_ct, round_to = NULL)
    expect_false(all(result1[["med_iqr"]] == result3[["med_iqr"]]))
})

# get_count() ####
test_that("get_count() works when provided categorical data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count(test_ct))
    result <- get_count(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 24)
    expect_true(is.numeric(result[["count"]]))

    wide_result <- to_wide(result, desc_col = "variable", cohort_col = "cohort")
    wide_all <- wide_result[["All"]]
    wide_cohort <- rowSums(wide_result[c(character_levels, "NA")])
    expect_equal(wide_all, wide_cohort)
})

test_that("get_count() works when provided multianswer data",{
    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count(test_ct))
    result <- get_count(test_ct)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 36)
    expect_true(is.numeric(result[["count"]]))

    wide_result <- to_wide(result, desc_col = "variable", cohort_col = "cohort")
    wide_all <- wide_result[["All"]]
    wide_cohort <- rowSums(wide_result[c(character_levels, "NA")])
    expect_equal(wide_all, wide_cohort)

    expect_gt(sum(wide_all), nrow(test_df))
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

# get_percent() ####
test_that("get_percent() works when provided categorical data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent(test_ct))
    result <- get_percent(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 24)
    expect_true(is.numeric(result[["percent"]]))

    wide_result <- to_wide(result, desc_col = "variable", cohort_col = "cohort")
    keep <- !is.na(wide_result[["variable"]])
    wide_result <- wide_result[keep, , drop = F]
    totals <- sapply(wide_result[c("All", character_levels, "NA")], sum)
    expect_true(all(totals > 98 & totals < 102))
})

test_that("get_percent() works when provided multianswer data",{
    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent(test_ct))
    result <- get_percent(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 36)
    expect_true(is.numeric(result[["percent"]]))

    wide_result <- to_wide(result, desc_col = "variable", cohort_col = "cohort")
    keep <- !is.na(wide_result[["variable"]])
    wide_result <- wide_result[keep, , drop = F]
    totals <- sapply(wide_result[c("All", character_levels, "NA")], sum)
    expect_false(all(totals > 98 & totals < 102))
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

    expect_silent(get_percent(test_ct, round_to = NULL))
    result3 <- get_percent(test_ct, round_to = NULL)
    expect_false(all(result1[["percent"]] == result3[["percent"]]))
})

# get_percent_str() ####
test_that("get_percent_str() works when provided categorical data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent_str(test_ct))
    result <- get_percent_str(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 24)
    expect_true(is.character(result[["percent_str"]]))
})

test_that("get_percent_str() works when provided multianswer data",{
    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent_str(test_ct))
    result <- get_percent_str(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 36)
    expect_true(is.character(result[["percent_str"]]))
})

test_that("get_percent_str() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent_str(test_ct, out_col_name = "test_col"))
    result <- get_percent_str(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_in("test_col", names(result))
})

test_that("get_percent_str() respects round_to",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_percent_str(test_ct, round_to = 0))
    result1 <- get_percent_str(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 3)

    expect_silent(get_percent_str(test_ct, round_to = 7))
    result2 <- get_percent_str(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))
    expect_true(all(result1[["variable"]] == result2[["variable"]], na.rm = T))

    percents1 <- result1[["percent_str"]]
    percents2 <- result2[["percent_str"]]

    expect_false(all(percents1 == percents2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- percents1[!is.na(percents1)]
    expect_false(any(grepl("\\.", non_na_vals)))

    expect_silent(get_percent_str(test_ct, round_to = NULL))
    result3 <- get_percent_str(test_ct, round_to = NULL)
    expect_false(all(result1[["percent_str"]] == result3[["percent_str"]]))
})

# get_count_percent() ####
test_that("get_count_percent() works when provided categorical data",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count_percent(test_ct))
    result <- get_count_percent(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 24)
    expect_true(is.character(result[["count_percent"]]))
})

test_that("get_count_percent() works when provided multianswer data",{
    test_df <- multi_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count_percent(test_ct))
    result <- get_count_percent(test_ct, round_to = 5)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_equal(nrow(result), 36)
    expect_true(is.character(result[["count_percent"]]))
})

test_that("get_count_percent() respects out_col_name",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count_percent(test_ct, out_col_name = "test_col"))
    result <- get_count_percent(test_ct, out_col_name = "test_col")

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_in("test_col", names(result))
})

test_that("get_count_percent() respects round_to",{
    test_df <- cat_test_df()
    test_ct <- crosstab(test_df, "cohort")

    expect_silent(get_count_percent(test_ct, round_to = 0))
    result1 <- get_count_percent(test_ct, round_to = 0)

    expect_s3_class(result1, "data.frame")
    expect_equal(ncol(result1), 3)

    expect_silent(get_count_percent(test_ct, round_to = 7))
    result2 <- get_count_percent(test_ct, round_to = 7)

    expect_true(all(result1[["cohort"]] == result2[["cohort"]], na.rm = T))
    expect_true(all(result1[["variable"]] == result2[["variable"]], na.rm = T))

    percents1 <- result1[["count_percent"]]
    percents2 <- result2[["count_percent"]]

    expect_false(all(percents1 == percents2, na.rm = T))

    # Make sure there are no periods in the one rounded to 0 decimal places
    non_na_vals <- percents1[!is.na(percents1)]
    expect_false(any(grepl("\\.", non_na_vals)))

    expect_silent(get_count_percent(test_ct, round_to = NULL))
    result3 <- get_count_percent(test_ct, round_to = NULL)
    expect_false(all(result1[["count_percent"]] == result3[["count_percent"]]))
})
