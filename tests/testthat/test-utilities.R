test_that("pass() returns the object unchanged", {
    x <- 1:5
    expect_identical(pass(x), x)

    y <- list(a = 1, b = 2)
    expect_identical(pass(y), y)

    z <- data.frame(a = c(1, 2, 3))
    expect_identical(pass(z), z)

    null_obj <- NULL
    expect_identical(pass(null_obj), NULL)
})

test_that("remove_na() removes NA values from vector", {
    x <- c(1, NA, 2, NA, 3)
    expect_equal(remove_na(x), c(1, 2, 3))
})

test_that("remove_na() works with all-NA vector", {
    x <- c(NA, NA)
    expect_equal(remove_na(x), logical(0))

    x <- c(NA, NA)
    expect_equal(as.numeric(remove_na(x)), numeric(0))
})

test_that("remove_na() works with no NA values", {
    x <- c(4, 5, 6)
    expect_equal(remove_na(x), x)
})

test_that("remove_na() works on character vector", {
    x <- c("a", NA, "b")
    expect_equal(remove_na(x), c("a", "b"))
})

test_that("remove_na() works on logical vector", {
    x <- c(TRUE, NA, FALSE)
    expect_equal(remove_na(x), c(TRUE, FALSE))
})

test_that("remove_na() works with empty vector", {
    expect_equal(remove_na(numeric(0)), numeric(0))
})

test_that("default_var_map() works on proper input",{
    fct <- factor(c("Agree", "Neither", "Disagree"))
    map <- default_var_map(fct)
    ref <- c("Agree" = 3, "Neither" = 2, "Disagree" = 1)
    expect_equal(map, ref)
})

test_that("default_var_map() errors when given a non-factor object",{
    expect_error(default_var_map(c("Agree", "Neither", "Disagree")))
    expect_error(default_var_map(c(1, 2, 3)))
    expect_error(default_var_map(NA))
})

test_that("get_non_matching() errors when the arguments aren't characters",{
    expect_error(get_non_matching(1, "a"))
    expect_error(get_non_matching(NULL, "a"))
    expect_error(get_non_matching(NA, "a"))
    expect_error(get_non_matching(TRUE, "a"))
    expect_error(get_non_matching(data.frame(), "a"))
    expect_error(get_non_matching("a", 1))
    expect_error(get_non_matching("a", NULL))
    expect_error(get_non_matching("a", NA))
    expect_error(get_non_matching("a", TRUE))
    expect_error(get_non_matching("a", data.frame()))
})

test_that("get_non_matching() returns the same string when there is no clash",{
    expect_equal(get_non_matching("a", "b"), "a")
    expect_equal(get_non_matching("abc", c("a", "b", "c")), "abc")
})

test_that("get_non_matching() warns when there is a clash and returns an ammended string",{
    expect_warning(get_non_matching("a", "a"))
    result <- suppressWarnings(get_non_matching("a", "a"))
    expect_equal(result, paste0("a", STR_CLASH_SUFFIX))
})

test_that("determine_col_type() errors when provided with incorrect data types",{
    expect_error(determine_col_type(NULL, var_map = NULL))
    expect_error(determine_col_type(1, var_map = c(1, 2, 3)))
    expect_error(determine_col_type(1, var_map = c(a = "a", b = "b", c = "c")))
})

test_that("determine_col_type() returns the proper types", {
    map <- c(a = 1, b = 2, c = 3)
    map2 <- c("TRUE" = 1, "FALSE" = 2)

    # Expect no errors
    expect_silent(determine_col_type(1, NULL))
    expect_silent(determine_col_type("a", NULL))
    expect_silent(determine_col_type(TRUE, NULL))
    expect_silent(determine_col_type(list(), NULL))
    expect_silent(determine_col_type(1, map))
    expect_silent(determine_col_type("a", map))
    expect_silent(determine_col_type("A", map))
    expect_silent(determine_col_type(TRUE, map))
    expect_silent(determine_col_type(TRUE, map2))
    expect_silent(determine_col_type(list(), map))

    # Expect correct return values
    expect_equal(determine_col_type(1, NULL),      CT_DATA_CLASS_NUM)
    expect_equal(determine_col_type("a", NULL),    CT_DATA_CLASS_CAT)
    expect_equal(determine_col_type(TRUE, NULL),   CT_DATA_CLASS_CAT)
    expect_equal(determine_col_type(list(), NULL), CT_DATA_CLASS_MULTI)

    expect_equal(determine_col_type(1, map),        CT_DATA_CLASS_NUM)
    expect_equal(determine_col_type("a", map),      CT_DATA_CLASS_LIKERT)
    expect_equal(determine_col_type("A", map),      CT_DATA_CLASS_CAT)
    expect_equal(determine_col_type(TRUE, map),     CT_DATA_CLASS_CAT)
    expect_equal(determine_col_type(TRUE, map2),    CT_DATA_CLASS_LIKERT)
    expect_equal(determine_col_type(list(), map),   CT_DATA_CLASS_MULTI)
})

test_that("nest_multi_col works with repeated rows", {
    df <- data.frame(
        id = c(1, 1, 2, 2, 2),
        allergy = c("peanuts", "shellfish", "gluten", "soy", "peanuts"),
        stringsAsFactors = FALSE
    )

    nested <- nest_multi_col(df, "allergy")

    expect_equal(nrow(nested), 2)
    expect_equal(nested$allergy[[1]], c("peanuts", "shellfish"))
    expect_equal(nested$allergy[[2]], c("gluten", "soy", "peanuts"))

    expect_true(is.list(nested$allergy))
    expect_true(all(vapply(nested$allergy, is.atomic, logical(1))))
})

test_that("nest_multi_col works with only one grouping column", {
    df <- data.frame(allergy = c("a", "b", "c", "a", "b"))

    result <- nest_multi_col(df, "allergy")

    expect_equal(nrow(result), 1)
    expect_true("allergy" %in% names(result))
    expect_true(is.list(result$allergy))
    expect_true(all(vapply(result$allergy, is.atomic, logical(1))))
})

test_that("nest_multi_col errors if input is not a data frame", {
    expect_error(nest_multi_col("not a df", "x"))
})

test_that("nest_multi_col errors if column name not found", {
    df <- data.frame(x = 1:3, y = 4:6)
    expect_error(nest_multi_col(df, "z"))
})

test_that("nest_multi_col preserves other columns", {
    df <- data.frame(
        school = c("A", "A", "B", "B"),
        student = c("x", "x", "y", "y"),
        allergy = c("nuts", "dairy", "soy", "gluten")
    )

    result <- nest_multi_col(df, "allergy")

    expect_equal(nrow(result), 2)
    expect_true(all(c("school", "student", "allergy") %in% names(result)))
    expect_equal(result$allergy[[1]], c("nuts", "dairy"))
})
