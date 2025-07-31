#' Detect Lists of Factors
#'
#' Drop-in replacement for [base::factor()] with added support for lists of
#' factors.
#'
#' This wrapper is a drop-in replacement for [base::factor()]. Because row and
#' column ordering is determined by factor levels, and multi-response data is
#' passed in as a list-column, these wrappers for [base::factor()] and
#' [base::levels()] add functionality to recursively apply to each item in a
#' list. This ensures that all values in a list have the same factor levels
#' without any [lapply()] nonsense.
#'
#' There is added functionality for the `levels` parameter of `factor()`.
#' Usually `levels` requires all values of factor() to be accounted for, but
#' this function will keep all levels and place the values in `levels` at the
#' front. Values you want at the end go in `end_levels`. Now, that said, if you
#' do want unaccounted levels to drop like the base functionality, set
#' `drop_levels = TRUE`.
#'
#' @param obj The vector or list you wish to factorize
#' @param levels Character vector - The levels to be placed at the front of the factor levels
#' @param end_levels Character vector - The levels to be placed at the end of the factor levels
#' @param drop_levels Logical - Whether levels not in `levels` or `end_levels` should be dropped
#' @param ... All other parameters to be passed to [base::factor()]
#' @param value The new levels vector to be applied to obj in `levels()` setter
#'
#' @returns `factor()` returns the object with factor levels applied, `levels()` returns the levels applied, and `is.factorlist()` returns a logical for whether it's a list of factors
#' @name factor_extensions
#'
#' @examples
#' sports_levels <- c("Soccer", "Football", "Basketball")
#' sports <- list(
#'     person_1 = c("Basketball", "Soccer"),
#'     person_2 = c("Soccer"),
#'     person_3 = c("Football", "Soccer")
#' )
#'
#' sports # Regular list of character vectors
#'
#' sports_factors <- htcrosstabs::factor(sports, end_levels = "Soccer")
#' sports_factors # Each item's levels contains all values
#'
#' htcrosstabs::levels(sports_factors) <- sports_levels # Sets levels
#' htcrosstabs::levels(sports_factors) # Extracts levels from the entire list
#'
#' is.factorlist(sports_factors)
#'
NULL

#' @describeIn factor_extensions Wrapper for [base::factor()] with support for lists
#' @export
factor <- function(obj, levels = NULL, end_levels = NULL, drop_levels = FALSE, ...) {
    # Clean empty values
    if (is.null(levels)) levels <- character()
    if (is.null(end_levels)) end_levels <- character()

    # Extract all values from obj
    all_values <- if (is.factorlist(obj)) {
        base::levels(unlist(obj))
    } else if (is.list(obj)) {
        unique(remove_na(unlist(obj)))
    } else if (base::is.factor(obj)) {
        base::levels(obj)
    } else {
        unique(remove_na(obj))
    }

    # Order values: levels at the start, then remaining, then end_levels at the end
    middle_values <- setdiff(all_values, c(levels, end_levels))
    final_levels <- c(levels, middle_values, end_levels)

    # Or, drop the remaining if drop_levels = TRUE
    if (drop_levels) {
        all_vals <- unique(c(levels, end_levels))
        final_levels <- final_levels[final_levels %in% all_vals]
    }

    # Add final_levels to ... (overriding levels if present)
    dot_args <- list(...)
    dot_args$levels <- final_levels

    # Apply factor logic
    if (is.list(obj)) {
        atomic <- sapply(obj, is.atomic)
        if (any(!atomic)) {
            warning(sprintf(
                "Detected non-atomic items in list-column:\n%s",
                paste(obj[!atomic], collapse = "\n")
            ))
        }
        result <- lapply(obj, function(x) do.call(base::factor, c(list(x), dot_args)))
    } else {
        result <- do.call(base::factor, c(list(obj), dot_args))
    }

    return(result)
}

#' @describeIn factor_extensions Wrapper for [base::levels()] with support for lists
#' @export
levels <- function(obj) {
    if (is.factorlist(obj))
        return(base::levels(unlist(obj)))
    else
        return(base::levels(obj))
}

#' @describeIn factor_extensions Wrapper for [base::`levels<-`()] with support for lists
#' @export
`levels<-` <- function(obj, value) {
    if (is.list(obj))
        return(lapply(obj, function(x) base::`levels<-`(x, value)))
    else
        return(base::`levels<-`(obj, value))
}

#' @describeIn factor_extensions Returns whether `obj` is a list of factors
#' @export
is.factorlist <- function(obj) {
    is.list(obj) && all(sapply(obj, function(x) is.factor(x)))
}
