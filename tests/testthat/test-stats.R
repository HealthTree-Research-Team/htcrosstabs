# p_value_categories() ####
test_that("p_value_categories returns expected strings for different p-values", {
    expect_equal(p_value_categories(NA), "NA")
    expect_equal(p_value_categories(0.0005), "< 0.001")
    expect_equal(p_value_categories(0.049), 0.049)
    expect_equal(p_value_categories(0.05), 0.05)
    expect_equal(p_value_categories(0.051), "NS (0.051)")
    expect_equal(p_value_categories(0.123456, cutoff = 0.1), "NS (0.123)")
    expect_equal(p_value_categories(0.0456789, round_to = 2), 0.05)
})

# remove_zero_rows() ####
test_that("remove_zero_rows removes only all-zero rows", {
    df1 <- data.frame(a = c(0, 1, 0), b = c(0, 2, 0))
    result1 <- remove_zero_rows(df1)
    expect_equal(nrow(result1), 1)
    expect_equal(result1, df1[2, , drop = FALSE])
})

test_that("remove_zero_rows returns empty df if all rows are zero", {
    df2 <- data.frame(a = c(0, 0), b = c(0, 0))
    result2 <- remove_zero_rows(df2)
    expect_equal(nrow(result2), 0)
    expect_equal(ncol(result2), 2)
})

test_that("remove_zero_rows returns original df if no zero rows", {
    df3 <- data.frame(a = c(1, 2), b = c(3, 4))
    result3 <- remove_zero_rows(df3)
    expect_equal(result3, df3)
})

# create_stat_row_skeleton() ####
test_that("create_stat_row_skeleton() creates the proper object", {
    test_ct <- crosstab(cat_test_df(), "cohort")

    expect_silent(create_stat_row_skeleton(test_ct))
    result <- create_stat_row_skeleton(test_ct)

    expected_output <- as.matrix(data.frame(
        Description = c("Dif. A", "Dif. B", "Dif. C", "Overall"),
        All = c("-", "-", "-", "-"),
        A = c("-", "-", "-", "-"),
        B = c("-", "-", "-", "-"),
        C = c("-", "-", "-", "-"),
        D = c("-", "-", "-", "-")
    ))
    rownames(expected_output) <- c("A", "B", "C", "Overall")

    expect_equal(result, expected_output)
})

# fill_stat_row_skeleton() ####
test_that("fill_stat_row_skeleton() fills the rows properly with insignificant differences",{
    seed <- 1234
    test_ct <- crosstab(num_test_df(seed = seed), "cohort")
    skeleton <- create_stat_row_skeleton(test_ct)

    result <- fill_stat_row_skeleton(
        new_rows = skeleton,
        data = test_ct,
        posthoc = get_anova_posthoc(test_ct),
        overall_p_value = get_anova_p_value(test_ct),
        cutoff = 0.03,
        round_to = 5
    )

    expect_equal(ncol(result), 6)
    expect_equal(nrow(result), 1)
})

test_that("fill_stat_row_skeleton() fills the rows properly with significant differences",{
    seed <- 231
    test_ct <- crosstab(num_test_df(seed = seed), "cohort")
    skeleton <- create_stat_row_skeleton(test_ct)

    result <- fill_stat_row_skeleton(
        new_rows = skeleton,
        data = test_ct,
        posthoc = get_tukey_posthoc(test_ct),
        overall_p_value = get_anova_p_value(test_ct),
        cutoff = 0.03,
        round_to = 5
    )

    expect_equal(ncol(result), 6)
    expect_equal(nrow(result), 4)
})

# get_markers() ####
test_that("get_markers returns symbols by default", {
    result <- get_markers(3)
    expect_equal(result, c("<sup>*</sup>", "<sup>&dagger;</sup>", "<sup>&Dagger;</sup>"))
})

test_that("get_markers returns alphabet markers", {
    result <- get_markers(2, "alphabet")
    expect_equal(result, c("<sup>a</sup>", "<sup>b</sup>"))
})

test_that("get_markers returns number markers", {
    result <- get_markers(3, "number")
    expect_equal(result, c("<sup>1</sup>", "<sup>2</sup>", "<sup>3</sup>"))
})

test_that("get_markers with superscript = FALSE", {
    expect_equal(get_markers(2, "alphabet", superscript = FALSE), c("a", "b"))
    expect_equal(get_markers(2, "number", superscript = FALSE), c("1", "2"))
    expect_equal(get_markers(2, "symbol", superscript = FALSE), c("*", "\u2020"))
})

test_that("get_markers errors on invalid marker_type", {
    expect_error(get_markers(2, "emoji"), "marker_type must be either")
})

test_that("get_markers handles num_markers = 0", {
    expect_null(get_markers(0, "symbol"))
})

#get_alphabet() ####
test_that("get_alphabet returns correct letters", {
    expect_equal(get_alphabet(3), c("<sup>a</sup>", "<sup>b</sup>", "<sup>c</sup>"))
})

test_that("get_alphabet with superscript = FALSE", {
    expect_equal(get_alphabet(2, superscript = FALSE), c("a", "b"))
})

test_that("get_alphabet with num_markers = 0 returns NULL", {
    expect_null(get_alphabet(0))
})

#get_numbers() ####
test_that("get_numbers returns correct numbers with superscript", {
    expect_equal(get_numbers(3), c("<sup>1</sup>", "<sup>2</sup>", "<sup>3</sup>"))
})

test_that("get_numbers without superscript", {
    expect_equal(get_numbers(2, superscript = FALSE), c("1", "2"))
})

test_that("get_numbers with num_markers = 0 returns NULL", {
    expect_null(get_numbers(0))
})

#get_symbols() ####
test_that("get_symbols returns correct HTML symbols with superscript", {
    expect_equal(get_symbols(2), c("<sup>*</sup>", "<sup>&dagger;</sup>"))
})

test_that("get_symbols returns Unicode symbols without superscript", {
    expect_equal(get_symbols(2, superscript = FALSE), c("*", "\u2020"))
})

test_that("get_symbols with num_markers = 0 returns NULL", {
    expect_null(get_symbols(0))
})

# extend_characters() ####
test_that("extend_characters extends base characters to required length", {
    expect_equal(extend_characters(letters, 3), c("a", "b", "c"))
    expect_equal(extend_characters(letters, 27)[27], "aa")
})

test_that("extend_characters with n = 0 returns NULL", {
    expect_null(extend_characters(letters, 0))
})

# multiply_characters() ####
test_that("multiply_characters repeats characters correctly", {
    expect_equal(multiply_characters(c("a", "b"), 3), c("aaa", "bbb"))
})

test_that("multiply_characters with n = 0 returns empty strings", {
    expect_equal(multiply_characters(c("a", "b"), 0), c("", ""))
})

# chisq_assumptions_met() ####
test_that("chisq_assumptions_met returns FALSE for empty data frame", {
    expect_false(chisq_assumptions_met(data.frame()))
    expect_false(chisq_assumptions_met(matrix(nrow = 0, ncol = 3)))
    expect_false(chisq_assumptions_met(matrix(nrow = 3, ncol = 0)))
})

test_that("chisq_assumptions_met returns TRUE when all expected counts >= 5", {
    df <- matrix(c(10, 10, 10,
                   10, 10, 10), nrow = 2, byrow = TRUE)
    expect_true(chisq_assumptions_met(df))
})

test_that("chisq_assumptions_met returns FALSE when any expected value < 5", {
    df <- matrix(c(1, 1, 1,
                   1, 1, 1), nrow = 2, byrow = TRUE)
    expect_false(chisq_assumptions_met(df))
})

test_that("chisq_assumptions_met works on data.frames too", {
    df <- as.data.frame(matrix(c(20, 20, 20, 20), nrow = 2))
    expect_true(chisq_assumptions_met(df))
})

test_that("chisq_assumptions_met handles non-square matrices", {
    df <- matrix(c(10, 10, 5, 5, 10, 10), nrow = 2)
    expect_true(chisq_assumptions_met(df))

    df_small <- matrix(c(1, 1, 1, 1, 1, 1), nrow = 2)
    expect_false(chisq_assumptions_met(df_small))
})

test_that("chisq_assumptions_met handles borderline expected values", {
    df <- matrix(c(5, 5, 5, 5), nrow = 2)
    expect_true(chisq_assumptions_met(df))
})

# get_anova() ####
test_that("get_anova() returns an anova object",{
    seed <- 1234
    test_ct <- crosstab(num_test_df(seed = seed), "cohort")

    expect_silent(get_anova(test_ct))
    result <- get_anova(test_ct)

    expect_s3_class(result, "aov")
})

# get_anova_p_value() ####
test_that("get_anova_p_value() returns the proper p-value",{
    seed <- 1234
    test_ct <- crosstab(num_test_df(seed = seed), "cohort")

    expect_silent(get_anova_p_value(test_ct))
    result <- get_anova_p_value(test_ct)

    expect_true(is.numeric(result))
    expect_equal(result, 0.045680831)
})

# get_tukey_posthoc() ####
test_that("get_tukey_posthoc() returns the data frame with p-values",{
    seed <- 1234
    test_ct <- crosstab(num_test_df(seed = seed), "cohort")

    expect_silent(get_tukey_posthoc(test_ct))
    result <- get_tukey_posthoc(test_ct)
    expect_true(is.data.frame(result))

    expect_equal(nrow(result), 4)
    expect_equal(ncol(result), 4)
    expect_equal(rownames(result), character_levels)
    expect_equal(colnames(result), character_levels)
})

# get_anova_markers() ####
test_that("get_anova_markers() returns a list of appropriate markers",{
    seed <- 134
    test_ct <- crosstab(num_test_df(seed = seed), "cohort")

    expect_silent(get_anova_markers(test_ct, character_levels))
    result <- get_anova_markers(test_ct, character_levels)

    expect_equal(result, list(
        A = NULL,
        B = "*",
        C = "\u2020",
        D = "\u2021"
    ))
})

# get_chisq() ####
test_that("get_chisq() returns a p_value",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")
    count_df <- get_count(test_ct) |>
        to_wide(var_name(test_ct), cohort_name(test_ct), na_fill = 0)
    count_df <- count_df[, character_levels, drop = F]

    expect_silent(get_chisq(count_df))
    result <- get_chisq(count_df)

    expect_true(is.numeric(result))
    expect_equal(result, 0.373535056)
})

# get_chisq_p_value() ####
test_that("get_chisq_p_value() returns the proper p-value",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")

    expect_silent(get_chisq_p_value(test_ct))
    result <- get_chisq_p_value(test_ct)

    expect_true(is.numeric(result))
    expect_equal(result, 0.373535056)
})

test_that("get_chisq_p_value() throws a warning when a cohort has no responses",{
    seed <- 1234
    test_df <- cat_test_df(seed = seed)

    # Create cohort with all NA repsonses
    d_cohort <- test_df[["cohort"]] == character_levels[4]
    test_df[["variable"]][d_cohort] <- NA

    test_ct <- crosstab(test_df, "cohort")
    expect_warning(get_chisq_p_value(test_ct))
})

# get_chisq_posthoc() ####
test_that("get_chisq_posthoc() returns the data frame with p-values",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")

    expect_silent(get_chisq_posthoc(test_ct))
    result <- get_chisq_posthoc(test_ct)
    expect_true(is.data.frame(result))

    expect_equal(nrow(result), 4)
    expect_equal(ncol(result), 4)
    expect_equal(rownames(result), character_levels)
    expect_equal(colnames(result), character_levels)
})

test_that("get_chisq_posthoc() respects p.adj",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")

    expect_error(get_chisq_posthoc(test_ct, p.adj = "this is not a logical value"))

    posthoc1 <- get_chisq_posthoc(test_ct, p.adj = T)
    posthoc2 <- get_chisq_posthoc(test_ct, p.adj = F)

    expect_false(identical(posthoc1, posthoc2))
})

test_that("get_chisq_posthoc() respects method",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")

    expect_error(get_chisq_posthoc(test_ct, method = "this is not a valid method"))

    posthoc1 <- get_chisq_posthoc(test_ct, method = "BH")
    posthoc2 <- get_chisq_posthoc(test_ct, method = "bonferroni")

    expect_false(identical(posthoc1, posthoc2))
})

# get_rao_scott() ####
test_that("get_rao_scott() returns a p_value",{
    seed <- 1234
    test_ct <- crosstab_data(multi_test_df(seed = seed), "cohort")

    expect_silent(get_rao_scott(get_raw_data(test_ct), "variable", "cohort"))
    result <- get_rao_scott(get_raw_data(test_ct), "variable", "cohort")

    expect_true(is.numeric(result))
    expect_equal(result, 0.554709128)
})

# get_rao_scott_p_value() ####
test_that("get_rao_scott_p_value() returns the proper p-value",{
    seed <- 1234
    test_ct <- crosstab(multi_test_df(seed = seed), "cohort")

    expect_silent(get_rao_scott_p_value(test_ct))
    result <- get_rao_scott_p_value(test_ct)

    expect_true(is.numeric(result))
    expect_equal(result, 0.554709128)
})

# get_rao_scott_posthoc() ####
test_that("get_rao_scott_posthoc() returns the data frame with p-values",{
    seed <- 1234
    test_ct <- crosstab(multi_test_df(seed = seed), "cohort")

    expect_silent(get_rao_scott_posthoc(test_ct))
    result <- get_rao_scott_posthoc(test_ct)
    expect_true(is.data.frame(result))

    expect_equal(nrow(result), 4)
    expect_equal(ncol(result), 4)
    expect_equal(rownames(result), character_levels)
    expect_equal(colnames(result), character_levels)
})

test_that("get_rao_scott_posthoc() respects p.adj",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")

    expect_error(get_rao_scott_posthoc(test_ct, p.adj = "this is not a logical value"))

    posthoc1 <- get_rao_scott_posthoc(test_ct, p.adj = T)
    posthoc2 <- get_rao_scott_posthoc(test_ct, p.adj = F)

    expect_false(identical(posthoc1, posthoc2))
})

test_that("get_rao_scott_posthoc() respects method",{
    seed <- 1234
    test_ct <- crosstab(cat_test_df(seed = seed), "cohort")

    expect_error(get_rao_scott_posthoc(test_ct, method = "this is not a valid method"))

    posthoc1 <- get_rao_scott_posthoc(test_ct, method = "BH")
    posthoc2 <- get_rao_scott_posthoc(test_ct, method = "bonferroni")

    expect_false(identical(posthoc1, posthoc2))
})
