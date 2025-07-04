# IMPORTS ####
#' @import assertthat

# IS CROSSTAB ####
#' @export
is.crosstab <- function(obj, strict = T) {
    if (is.null(obj)) return(FALSE)
    if (strict) return(inherits(obj, CT_CLASS))
    return(inherits(obj, c(CT_CLASS, CT_DATA_CLASS)))
}

#' @export
is.crosstab.data <- function(obj) {
    if (is.null(obj)) return(FALSE)
    return(inherits(obj, CT_DATA_CLASS))
}

#' @export
is.crosstab.categorical <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- get_data(obj)
    return(inherits(obj, CT_DATA_CLASS_CAT))
}

#' @export
is.crosstab.numeric <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- get_data(obj)
    return(inherits(obj, CT_DATA_CLASS_NUM))
}

#' @export
is.crosstab.likert <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- get_data(obj)
    return(inherits(obj, CT_DATA_CLASS_LIKERT))
}

#' @export
is.crosstab.multi <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- get_data(obj)
    return(inherits(obj, CT_DATA_CLASS_MULTI))
}

#' @export
is.crosstab.grouped <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is.crosstab(obj)) obj <- get_data(obj)
    return(inherits(obj, CT_DATA_CLASS_GROUPED))
}

# ASSERT CROSSTAB ####
#' @export
assert_crosstab <- function(obj, strict = T) {
    assert_that(is.crosstab(obj, strict), msg = "Not a crosstab object")
}

#' @export
assert_crosstab_data <- function(obj) {
    assert_that(is.crosstab.data(obj), msg = "Not a crosstab_data object")
}

#' @export
assert_crosstab_categorical <- function(obj) {
    assert_that(is.crosstab.categorical(obj), msg = "Not a categorical crosstab object")
}

#' @export
assert_crosstab_numeric <- function(obj) {
    assert_that(is.crosstab.numeric(obj), msg = "Not a numeric crosstab object")
}

#' @export
assert_crosstab_likert <- function(obj) {
    assert_that(is.crosstab.likert(obj), msg = "Not a likert crosstab object")
}

#' @export
assert_crosstab_multi <- function(obj) {
    assert_that(is.crosstab.multi(obj), msg = "Not a multianswer crosstab object")
}

#' @export
assert_crosstab_grouped <- function(obj) {
    assert_that(is.crosstab.grouped(obj), msg = "Not a grouped crosstab object")
}

# AS CATEGORICAL ####
#' @export
as.crosstab.cat <- function(ct_data) {
    UseMethod("as.crosstab.cat", ct_data)
}

#' @export
as.crosstab.cat.crosstab <- function(ct_data) {
    set_data(ct_data) <- as.crosstab.cat(get_data(ct_data))
    return(ct_data)
}

#' @export
as.crosstab.cat.crosstab_data_cat <- function(ct_data) {
    validate_crosstab_data.crosstab_data_cat(ct_data)
    return(ct_data)
}

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

#' @export
as.crosstab.cat.crosstab_data_likert <- function(ct_data) {

    # Get rid of the var_mapping attribute
    attr(ct_data, "var_mapping") <- NULL

    # Change the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_LIKERT] <- CT_DATA_CLASS_CAT

    # Validate you did it right
    validate_crosstab_data.crosstab_data_cat(ct_data)

    return(ct_data)
}

#' @export
as.crosstab.cat.crosstab_data_multi <- function(ct_data) {

    # Unnest the list column
    variable_name <- var_name(ct_data)
    unnested_data <- ct_data |>
        tidyr::unnest(
            {{variable_name}},
            keep_empty = T
        ) |>
        data.frame(check.names = F)

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

# AS NUMERIC ####
#' @export
as.crosstab.num <- function(ct_data) {
    UseMethod("as.crosstab.num", ct_data)
}

#' @export
as.crosstab.num.crosstab <- function(ct_data) {
    set_data(ct_data) <- as.crosstab.num(get_data(ct_data))
    return(ct_data)
}

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

#' @export
as.crosstab.num.crosstab_data_num <- function(ct_data) {
    validate_crosstab_data.crosstab_data_num(ct_data)
    return(ct_data)
}

#' @export
as.crosstab.num.crosstab_data_likert <- function(ct_data) {

    # Convert likert variables to numbers via their mapping
    ct_data[[var_name(ct_data)]] <- var_mapped(ct_data)

    # Get rid of unneeded attributes
    attr(ct_data, "var_levels") <- NULL
    attr(ct_data, "var_mapping") <- NULL

    # Change the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_LIKERT] <- CT_DATA_CLASS_NUM

    # Double check you created a valid object
    validate_crosstab_data.crosstab_data_num(ct_data)

    return(ct_data)
}

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

# AS LIKERT ####
#' @export
as.crosstab.likert <- function(ct_data, likert_map = NULL) {
    UseMethod("as.crosstab.likert", ct_data)
}

#' @export
as.crosstab.likert.crosstab <- function(ct_data, likert_map = NULL) {
    set_data(ct_data) <- as.crosstab.likert(get_data(ct_data), likert_map = likert_map)
    return(ct_data)
}

#' @export
as.crosstab.likert.crosstab_data_cat <- function(ct_data, likert_map = NULL) {
    validate_as_crosstab_likert(ct_data, likert_map)

    # Add the var_mapping attribute
    attr(ct_data, "var_mapping") <- likert_map

    # Add the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_CAT] <- CT_DATA_CLASS_LIKERT

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_likert(ct_data)

    return(ct_data)
}

#' @export
as.crosstab.likert.crosstab_data_num <- function(ct_data, likert_map = NULL) {
    validate_as_crosstab_likert(ct_data, likert_map)

    # Map the numbers to their corresponding names in likert_map
    ct_data[[var_name(ct_data)]] <- names(likert_map)[match(var(ct_data), likert_map)]

    # Add the missing attributes
    attr(ct_data, "var_levels") <- names(likert_map)
    attr(ct_data, "var_mapping") <- likert_map

    # Add the class
    class(ct_data)[class(ct_data) == CT_DATA_CLASS_NUM] <- CT_DATA_CLASS_LIKERT

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_likert(ct_data)

    return(ct_data)
}

#' @export
as.crosstab.likert.crosstab_data_likert <- function(ct_data, likert_map = NULL) {
    if(!is.null(likert_map))
        var_mapping(ct_data) <- likert_map
    validate_crosstab_data.crosstab_data_likert(ct_data)
    return(ct_data)
}

#' @export
as.crosstab.likert.crosstab_data_multi <- function(ct_data, likert_map = NULL) {
    validate_as_crosstab_likert(ct_data, likert_map)

    # Unnest the values
    cat_data <- as.crosstab.cat(ct_data)

    # Convert the resulting categorical table to likert
    likert_data <- as.crosstab.likert(cat_data, likert_map = likert_map)

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_likert(likert_data)

    return(likert_data)
}

# AS MULTIANSWER ####
#' @export
as.crosstab.multi <- function(ct_data) {
    UseMethod("as.crosstab.multi", ct_data)
}

#' @export
as.crosstab.multi.crosstab <- function(ct_data) {
    set_data(ct_data) <- as.crosstab.multi(get_data(ct_data))
    return(ct_data)
}

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

#' @export
as.crosstab.multi.crosstab_data_likert <- function(ct_data) {

    # Strip off the var_mapping attribute
    cat_data <- as.crosstab.cat(ct_data)

    # Convert the variable column to a list column
    multi_data <- as.crosstab.multi(cat_data)

    # Validate to make sure you did it right
    validate_crosstab_data.crosstab_data_multi(multi_data)

    return(multi_data)
}

#' @export
as.crosstab.multi.crosstab_data_multi <- function(ct_data) {
    validate_crosstab_data.crosstab_data_multi(ct_data)
    return(ct_data)
}
