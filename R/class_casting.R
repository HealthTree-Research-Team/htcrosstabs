# CROSSTAB CHECKS ####

#' Check or Assert Crosstab Object Types
#'
#' `is.crosstab()` and related functions test or assert the structure of a
#' crosstab object and its subclasses (e.g., numeric, categorical, likert,
#' multi-response, grouped). return `TRUE` if the object is a valid crosstab
#' (or specific subtype). The `assert_` functions will throw an error
#' if the check fails.
#'
#' @param obj An object to check.
#' @param strict Logical. If `TRUE`, only returns `TRUE` for fully constructed
#'   crosstab objects (will return false for raw data subclasses).
#'
#' @returns Logical for `is.*()` and `assert_*()` functions (`assert_*()` functions throw error if check fails).
#'
#' @name check_crosstab
#'
#' @import assertthat
#' @examples
#' # Numeric, no grouping column
#' num_ct_ungrouped <- crosstab(petal_width)
#'
#' is.crosstab(iris) # FALSE
#' is.crosstab(num_ct_ungrouped) # TRUE
#' is.crosstab.categorical(num_ct_ungrouped) # FALSE
#' is.crosstab.numeric(num_ct_ungrouped) # TRUE
#'
#' # Numeric, grouped
#' num_ct_grouped <- crosstab(length_by_species, "species")
#'
#' is.crosstab.grouped(num_ct_ungrouped) # FALSE
#' is.crosstab.grouped(num_ct_grouped) # TRUE
#'
#' # Differentiating complete crosstabs from inner data objects
#' num_ct_data <- crosstab_data(length_by_species, "species")
#'
#' is.crosstab.data(num_ct_ungrouped) # FALSE
#' is.crosstab.data(num_ct_data) # TRUE
#' is.crosstab(num_ct_data) # FALSE (only fully constructed crosstabs return true)
#' is.crosstab(num_ct_data, strict = FALSE) # TRUE (any part of a crosstab returns TRUE)
NULL

#' @describeIn check_crosstab Checks if the data is a crosstab object
#' @export
is.crosstab <- function(obj, strict = TRUE) {
    if (is.null(obj)) return(FALSE)
    if (strict) return(inherits(obj, CT_CLASS))
    return(inherits(obj, c(CT_CLASS, CT_DATA_CLASS)))
}

#' @describeIn check_crosstab Checks if the data is specifically a crosstab_data object
#' @export
is.crosstab.data <- function(obj) {
    if (is.null(obj)) return(FALSE)
    return(inherits(obj, CT_DATA_CLASS))
}

#' @describeIn check_crosstab Checks if the data is categorical
#' @export
is.crosstab.categorical <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- data_table(obj)
    return(inherits(obj, CT_DATA_CLASS_CAT))
}

#' @describeIn check_crosstab Checks if the data is numeric
#' @export
is.crosstab.numeric <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- data_table(obj)
    return(inherits(obj, CT_DATA_CLASS_NUM))
}

#' @describeIn check_crosstab Checks if the data is Likert-like
#' @export
is.crosstab.likert <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- data_table(obj)
    return(inherits(obj, CT_DATA_CLASS_LIKERT))
}

#' @describeIn check_crosstab Checks if the data is multi-response
#' @export
is.crosstab.multi <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- data_table(obj)
    return(inherits(obj, CT_DATA_CLASS_MULTI))
}

#' @describeIn check_crosstab Checks if the data is grouped
#' @export
is.crosstab.grouped <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- data_table(obj)
    return(inherits(obj, CT_DATA_CLASS_GROUPED))
}

#' @describeIn check_crosstab Throws error if the data is not a crosstab
#' @export
assert_crosstab <- function(obj, strict = TRUE) {
    assert_that(is.crosstab(obj, strict), msg = "Not a crosstab object")
}

#' @describeIn check_crosstab Throws error if the data is not a crosstab_data
#' @export
assert_crosstab_data <- function(obj) {
    assert_that(is.crosstab.data(obj), msg = "Not a crosstab_data object")
}

#' @describeIn check_crosstab Throws error if the data is not categorical
#' @export
assert_crosstab_categorical <- function(obj) {
    assert_that(is.crosstab.categorical(obj), msg = "Not a categorical crosstab object")
}

#' @describeIn check_crosstab Throws error if the data is not numeric
#' @export
assert_crosstab_numeric <- function(obj) {
    assert_that(is.crosstab.numeric(obj), msg = "Not a numeric crosstab object")
}

#' @describeIn check_crosstab Throws error if the data is not Likert-like
#' @export
assert_crosstab_likert <- function(obj) {
    assert_that(is.crosstab.likert(obj), msg = "Not a likert crosstab object")
}

#' @describeIn check_crosstab Throws error if the data is not multi-response
#' @export
assert_crosstab_multi <- function(obj) {
    assert_that(is.crosstab.multi(obj), msg = "Not a multianswer crosstab object")
}

#' @describeIn check_crosstab Throws error if the data is not grouped
#' @export
assert_crosstab_grouped <- function(obj) {
    assert_that(is.crosstab.grouped(obj), msg = "Not a grouped crosstab object")
}

# CROSSTAB CASTING ####
#' Cast Crosstab Object to New Data Type
#'
#' `as.crosstab.*()` functions take preexisting data and convert it to
#' a new type of crosstab object (e.g. numeric -> categorical, likert ->
#' numeric, etc.)
#'
#' @section Converting to Categorical:
#' Numeric values are simply cast using `as.factor()`, internally storing them
#' as their character representations.
#'
#' Likert-like data is already categorical, but with extra mapping
#' functionality, bestowed by `var_map`. Mapping functionality is dropped when
#' casting.
#'
#' Multi-response data simply has its list-column unnested into one answer per
#' row.
#'
#' @section Converting to Numeric:
#' Categorical values are cast using `as.numeric()`, which replaces the data
#' with its numeric factor level.
#'
#' Likert-like data has `var_map` for this exact purpose. The character values
#' are found in the names of `var_map` and then mapped to their corresponding
#' numeric values.
#'
#' Multi-response data is unnested to one answer per row, and then undergoes
#' the same process as categorical data.
#'
#' @section Converting to Likert-Like:
#' Categorical values are kept, and the provided `var_map` is attached to the
#' crosstab object.
#'
#' Numeric data is reverse-mapped to categorical values by their values in
#' `var_map`.
#'
#' Multi-response data is unnested to one response per row and then the same
#' as categorical values.
#'
#' @section Converting to Multi-Response:
#' Categorical values are turned into a list-column where each row still has
#' one answer per row.
#'
#' Numeric values are cast using `as.factor()` and then the same as categorical
#' values.
#'
#' Likert-like data has "mapping" functionality dropped and then the same as
#' categorical values.
#'
#' @param ct_data The data or crosstab object to be cast
#' @param var_map A named numeric vector mapping character values to numeric values
#'
#' @returns The converted crosstab object
#' @name cast_crosstab
#'
#' @examples
#' num_ct <- crosstab(length_by_species, "species")
#' converted_ct <- as.crosstab.cat(num_ct)
#' is.crosstab.categorical(converted_ct) # TRUE
#'
#' # Likert data requires a map
#' cat_ct <- crosstab(licorice_by_region, "region")
#' licorice_map <- c("likes" = 1, "neither" = 0, "dislikes" = -1)
#' converted_ct <- as.crosstab.likert(cat_ct, var_map = licorice_map)
#' is.crosstab.likert(converted_ct) # TRUE
NULL

#' @describeIn cast_crosstab Casts the crosstab object to categorical
#' @export
as.crosstab.cat <- function(ct_data) {
    UseMethod("as.crosstab.cat", ct_data)
}

#' @noRd
#' @export
as.crosstab.cat.crosstab <- function(ct_data) {
    data_table(ct_data) <- as.crosstab.cat(data_table(ct_data))
    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.cat.crosstab_data_cat <- function(ct_data) {
    validate_crosstab_data.crosstab_data_cat(ct_data)
    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.cat.crosstab_data_num <- function(ct_data) {
    # Get the factor levels
    var_levels <- var(ct_data) |>
        unique() |>
        sort()

    # Add the var_levels attribute
    attr(ct_data, "var_levels") <- var_levels

    # Convert the data to a factor
    ct_data[[var_name(ct_data)]] <- factor(ct_data[[var_name(ct_data)]], levels = var_levels)

    # Change the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_NUM] <- CT_DATA_CLASS_CAT

    # Validate you did it properly
    validate_crosstab_data.crosstab_data_cat(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.cat.crosstab_data_likert <- function(ct_data) {

    # Get rid of the var_map attribute
    attr(ct_data, "var_map") <- NULL

    # Change the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_LIKERT] <- CT_DATA_CLASS_CAT

    # Validate you did it right
    validate_crosstab_data.crosstab_data_cat(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.cat.crosstab_data_multi <- function(ct_data) {

    # Unnest the list column
    variable_name <- var_name(ct_data)
    unnested_data <- ct_data |>
        tidyr::unnest(
            {{variable_name}},
            keep_empty = TRUE
        ) |>
        data.frame(check.names = FALSE)

    # Transfer the attributes from ct_data to unnested_data
    orig_attrs <- attributes(ct_data)
    to_restore <- setdiff(names(orig_attrs), c("row.names", "names", "class"))
    for (attr_name in to_restore)
        attr(unnested_data, attr_name) <- orig_attrs[[attr_name]]
    class(unnested_data) <- class(ct_data)

    # Change the class
    class(unnested_data)[class(unnested_data) == CT_DATA_CLASS_MULTI] <- CT_DATA_CLASS_CAT

    # Validate you did it properly
    validate_crosstab_data.crosstab_data_cat(unnested_data)

    return(unnested_data)
}

#' @describeIn cast_crosstab Casts the crosstab object to numeric
#' @export
as.crosstab.num <- function(ct_data) {
    UseMethod("as.crosstab.num", ct_data)
}

#' @noRd
#' @export
as.crosstab.num.crosstab <- function(ct_data) {
    data_table(ct_data) <- as.crosstab.num(data_table(ct_data))
    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.num.crosstab_data_cat <- function(ct_data) {

    # Drop the var_levels attribute
    attr(ct_data, "var_levels") <- NULL

    # Convert categorical variables to numbers via their factor levels
    ct_data[[var_name(ct_data)]] <- as.numeric(ct_data[[var_name(ct_data)]])

    # Add the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_CAT] <- CT_DATA_CLASS_NUM

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_num(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.num.crosstab_data_num <- function(ct_data) {
    validate_crosstab_data.crosstab_data_num(ct_data)
    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.num.crosstab_data_likert <- function(ct_data) {

    # Convert likert variables to numbers via their mapping
    ct_data[[var_name(ct_data)]] <- var_mapped(ct_data)

    # Get rid of unneeded attributes
    attr(ct_data, "var_levels") <- NULL
    attr(ct_data, "var_map") <- NULL

    # Change the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_LIKERT] <- CT_DATA_CLASS_NUM

    # Double check you created a valid object
    validate_crosstab_data.crosstab_data_num(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.num.crosstab_data_multi <- function(ct_data) {

    # Unnest the data
    cat_data <- as.crosstab.cat(ct_data)

    # Convert the factors to their numeric value
    num_data <- as.crosstab.num(cat_data)

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_num(num_data)

    return(num_data)
}

#' @describeIn cast_crosstab Casts the crosstab object to Likert-like
#' @export
as.crosstab.likert <- function(ct_data, var_map = NULL) {
    UseMethod("as.crosstab.likert", ct_data)
}

#' @noRd
#' @export
as.crosstab.likert.crosstab <- function(ct_data, var_map = NULL) {
    data_table(ct_data) <- as.crosstab.likert(data_table(ct_data), var_map = var_map)
    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.likert.crosstab_data_cat <- function(ct_data, var_map = NULL) {
    validate_input_as_likert(ct_data, var_map)

    # Add the var_map attribute
    attr(ct_data, "var_map") <- var_map

    # Add the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_CAT] <- CT_DATA_CLASS_LIKERT

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_likert(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.likert.crosstab_data_num <- function(ct_data, var_map = NULL) {
    validate_input_as_likert(ct_data, var_map)

    # Map the numbers to their corresponding names in var_map
    ct_data[[var_name(ct_data)]] <- names(var_map)[match(var(ct_data), var_map)]

    # Factorize
    factor_levels <- sort(var_map, decreasing = TRUE)
    factor_levels <- names(factor_levels)
    ct_data[[var_name(ct_data)]] <- factor(ct_data[[var_name(ct_data)]], levels = factor_levels)

    # Add the missing attributes
    attr(ct_data, "var_levels") <- names(var_map)
    attr(ct_data, "var_map") <- var_map

    # Add the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_NUM] <- CT_DATA_CLASS_LIKERT

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_likert(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.likert.crosstab_data_likert <- function(ct_data, var_map = NULL) {
    validate_input_as_likert(ct_data, var_map)

    if (!is.null(var_map))
        var_map(ct_data) <- var_map

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.likert.crosstab_data_multi <- function(ct_data, var_map = NULL) {
    validate_input_as_likert(ct_data, var_map)

    # Unnest the values
    cat_data <- as.crosstab.cat(ct_data)

    # Convert the resulting categorical table to likert
    likert_data <- as.crosstab.likert(cat_data, var_map = var_map)

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_likert(likert_data)

    return(likert_data)
}

#' @describeIn cast_crosstab Casts the crosstab object to multi-answer
#' @export
as.crosstab.multi <- function(ct_data) {
    UseMethod("as.crosstab.multi", ct_data)
}

#' @noRd
#' @export
as.crosstab.multi.crosstab <- function(ct_data) {
    data_table(ct_data) <- as.crosstab.multi(data_table(ct_data))
    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.multi.crosstab_data_cat <- function(ct_data) {

    # Turn the variable column into a list column
    ct_data[[var_name(ct_data)]] <- sapply(var(ct_data), list)

    # Change the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_CAT] <- CT_DATA_CLASS_MULTI

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_multi(ct_data)

    return(ct_data)
}

#' @noRd
#' @export
as.crosstab.multi.crosstab_data_num <- function(ct_data) {

    # Turn the numbers into factors
    cat_data <- as.crosstab.cat(ct_data)

    # Convert the variable column to a list column
    multi_data <- as.crosstab.multi(cat_data)

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_multi(multi_data)

    return(multi_data)
}

#' @noRd
#' @export
as.crosstab.multi.crosstab_data_likert <- function(ct_data) {

    # Strip off the var_map attribute
    cat_data <- as.crosstab.cat(ct_data)

    # Convert the variable column to a list column
    multi_data <- as.crosstab.multi(cat_data)

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_multi(multi_data)

    return(multi_data)
}

#' @noRd
#' @export
as.crosstab.multi.crosstab_data_multi <- function(ct_data) {
    validate_crosstab_data.crosstab_data_multi(ct_data)
    return(ct_data)
}
