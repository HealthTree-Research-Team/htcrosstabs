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

test_that("factor() behaves like base::factor for atomic vectors", {
    x <- c("a", "b", "a", "c")
    expect_equal(factor(x), base::factor(x))

    y <- c("yes", "no", "yes")
    expect_equal(factor(y, levels = c("yes", "no")), base::factor(y, levels = c("yes", "no")))
})

test_that("factor() orders levels correctly using levels and end_levels", {
    x <- c("a", "b", "c", "d")
    f <- factor(x, levels = c("b", "a"), end_levels = c("d"))
    expect_equal(levels(f), c("b", "a", "c", "d"))
})

test_that("factor() applies factor() to each list element", {
    x <- list(c("a", "b"), "b", c("c", "a", "d"), NA)
    f <- factor(x, levels = c("a"), end_levels = c("d"))

    expect_true(all(sapply(f, is.factor)))

    expect_equal(levels(f[[1]]), c("a", "b", "c", "d"))
    expect_equal(levels(f[[3]]), c("a", "b", "c", "d"))
})

test_that("factor() handles NA values correctly", {
    x <- c("a", NA, "b", "c")
    f <- factor(x, levels = c("a"))
    expect_true(is.factor(f))
    expect_true(any(is.na(f)))
    expect_equal(levels(f), c("a", "b", "c"))

    x_list <- list("a", NA, c("b", "c"))
    f_list <- factor(x_list, levels = c("a"))
    expect_equal(levels(f_list[[3]]), c("a", "b", "c"))
})

test_that("factor() preserves factor behavior on existing factor", {
    f <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))
    new_f <- factor(f, levels = c("medium"))
    expect_equal(levels(new_f), c("medium", "low", "high"))
})

test_that("factor() works with empty input", {
    expect_equal(factor(character(0)), factor(character(0)))
    expect_equal(factor(list()), list())
})

test_that("factor() returns list with consistent levels", {
    x <- list(c("yes", "no"), "no", "maybe")
    f <- factor(x, levels = c("yes"), end_levels = c("maybe"))
    all_levels <- unique(unlist(lapply(f, levels)))
    expect_equal(all_levels, c("yes", "no", "maybe"))
})

test_that("factor() drops levels not specified when drop_levels = TRUE",{
    x <- c("a", "b", "c", "d", "e")
    f <- factor(x, levels = c("b", "a"), end_levels = c("e", "d"), drop_levels = T)
    expect_false("c" %in% f)
})

test_that("factor() applies factor() to every list in a list-column",{
    df <- multi_test_df(gr = T)
    df[["variable"]] <- factor(df[["variable"]], levels = c("badminton", "soccer"), end_levels = c("golf"))
    expect_true(is.factorlist(df[["variable"]]))
})

test_that("is.factorlist() returns TRUE for valid factor list", {
    x <- list(factor(c("a", "b")), factor("c"))
    expect_true(is.factorlist(x))
})

test_that("is.factorlist() returns FALSE for list with non-factor NA placeholder", {
    x <- list(factor("a"), NA)
    expect_false(is.factorlist(x))

    y <- list(NA, NA)
    expect_false(is.factorlist(y))
})

test_that("is.factorlist() returns TRUE for list with factorized NA placeholder", {
    x <- list(factor("a"), factor(NA))
    expect_true(is.factorlist(x))

    y <- list(factor(NA), factor(NA))
    expect_true(is.factorlist(y))
})

test_that("is.factorlist() returns FALSE for atomic vectors", {
    expect_false(is.factorlist(c("a", "b", "c")))
    expect_false(is.factorlist(factor(c("a", "b"))))
})

test_that("is.factorlist() returns FALSE for lists with non-factors", {
    x <- list("a", factor("b"))
    expect_false(is.factorlist(x))

    y <- list(1:3, factor("b"))
    expect_false(is.factorlist(y))

    z <- list(list("a"), factor("b"))  # Nested list
    expect_false(is.factorlist(z))
})

test_that("is.factorlist() handles empty lists correctly", {
    expect_true(is.factorlist(list()))
})

test_that("is.factorlist() handles list with NULLs", {
    expect_false(is.factorlist(list(NULL, factor("a"))))
    expect_false(is.factorlist(list(NULL)))
})

test_that("levels() works on single factor", {
    f <- factor(c("a", "b", "a"))
    expect_equal(levels(f), c("a", "b"))
})

test_that("levels() works on list of factors", {
    x <- list(factor(c("a", "b")), factor("b"), factor("c"))
    expect_equal(levels(x), c("a", "b", "c"))
})

test_that("levels() gives NULL on list of factors and non-factor NA", {
    x <- list(factor("a"), NA)
    expect_null(levels(x))
})

test_that("levels() works on list of factors and factorized NA", {
    x <- list(factor("a"), factor(NA))
    expect_equal(levels(x), c("a"))
})

test_that("levels() returns NULL when list has non-factor", {
    bad <- list(factor("a"), "b")
    expect_null(levels(bad))
})

test_that("levels<- works on single factor", {
    f <- factor(c("a", "b"))
    levels(f) <- c("x", "y")
    expect_equal(levels(f), c("x", "y"))
})

test_that("levels<- works on list of factors", {
    x <- list(factor("a"), factor("b"))
    x <- `levels<-`(x, c("x", "y"))
    expect_true(all(sapply(x, function(el) all(levels(el) == c("x", "y")))))
})

test_that("levels<- still applies base::levels() even if any item in list is not a factor, but doesn't factorize the list", {
    bad <- list(factor("a"), "b")
    expect_silent(`levels<-`(bad, c("x", "y")))
    expect_false(is.factorlist(`levels<-`(bad, c("x", "y"))))
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

