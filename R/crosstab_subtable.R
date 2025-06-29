#' @import assertthat

# CONSTANTS
# FIXME: Maybe name this crosstab_data?
CT_SUB_CLASS <- "crosstab_sub"
CT_SUB_MUL_CLASS <- "crosstab_sub_mul"

# CONSTRUCTORS
new_crosstab_sub_internal <- function(df, variable, cohort, variable_levels, cohort_levels, mul = F) {
    assert_that(is.data.frame(df))
    assert_that(is.character(variable))
    assert_that(length(variable) == 1, msg = "variable must only have one value")
    assert_that(is.character(cohort))
    assert_that(length(cohort) == 1, msg = "cohort must only have one value")
    assert_that(!is.null(cohort_levels), msg = "cohort_levels is null")
    assert_that(is.logical(mul))

    classes <- c(if (mul) CT_SUB_MUL_CLASS, CT_SUB_CLASS, class(df))

    structure(
        df,
        variable = variable,
        variable_levels = variable_levels,
        cohort = cohort,
        cohort_levels = cohort_levels,
        class = classes
    )
}

new_crosstab_sub <- function(df, variable, cohort, variable_levels, cohort_levels) {
    new_crosstab_sub_internal(df, variable, cohort, variable_levels, cohort_levels, mul = F)
}

new_crosstab_sub_multi <- function(df, variable, cohort, variable_levels, cohort_levels) {
    new_crosstab_sub_internal(df, variable, cohort, variable_levels, cohort_levels, mul = T)
}

# VALIDATOR
validate_crosstab_sub <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))

    assert_that(ncol(ct_sub) == 2, msg = "df must have 2 columns - variable and cohort")

    assert_that(has_attr(ct_sub, "variable"))
    variable = attr(ct_sub, "variable")
    assert_that(variable %in% names(ct_sub), msg = sprintf(
        "%s column not found in df",
        variable
    ))
    assert_that(!is.character(unlist(var(ct_sub))), msg = sprintf(
        "%s cannot be of type character, must be coerced to factor",
        variable
    ))

    if (inherits(ct_sub, CT_SUB_MUL_CLASS)) {
        assert_that(is.list(var(ct_sub)), msg = "Multi-answer data must be in a list-column")
    }

    if (is.factor(unlist(ct_sub[[variable]]))) {
        assert_that(has_attr(ct_sub, "variable_levels"), msg = sprintf(
            "%s is a factor column, but variable_levels is null",
            variable
        ))
        variable_levels = attr(ct_sub, "variable_levels")
        assert_that(all(unlist(var(ct_sub)) %in% c(variable_levels, NA)), msg = sprintf(
            "All values in %s must exist in variable_levels",
            variable
        ))
    }

    assert_that(has_attr(ct_sub, "cohort"))
    cohort = attr(ct_sub, "cohort")
    assert_that(cohort %in% names(ct_sub), msg = sprintf(
        "%s column not found in data",
        cohort
    ))
    assert_that(!is.character(unlist(cohort(ct_sub))), msg = sprintf(
        "%s cannot be of type character, must be coerced to factor",
        cohort
    ))

    assert_that(has_attr(ct_sub, "cohort_levels"))
    cohort_levels = attr(ct_sub, "cohort_levels")
    assert_that(all(cohort(ct_sub) %in% c(cohort_levels, NA)), msg = sprintf(
        "All values in %s must exist in cohort_levels",
        cohort
    ))

    return(TRUE)
}

# HELPER
crosstab_sub <- function(df, cohort_col_name = NULL, default_cohort = "Response") {
    assert_that(is.data.frame(df))

    # If it isn't grouped, add grouping column
    if (is.null(cohort_col_name)) {
        assert_that(ncol(df) == 1, msg = "If cohort_col_name is left NULL df must only have 1 column for data")

        # Create a cohort column name that doesn't already exist
        cohort_col_name = "cohort"
        while (cohort_col_name %in% names(df))
            cohort_col_name = paste0(cohort_col_name, "1")

        df[[cohort_col_name]] <- default_cohort
    } else {
        assert_that(ncol(df) == 2, msg = sprintf(
            "If providing cohort_col_name df must have 2 columns: one for data and one for cohort"
        ))
        assert_that(cohort_col_name %in% names(df), msg = sprintf(
            "Cohort column \"%s\" not found in df",
            cohort_col_name
        ))
    }

    variable_col_name <- names(df)[names(df) != cohort_col_name]

    # Warn if there are empty columns
    empty_cols <- names(df)[sapply(df, function(x) all(is.na(x)))]
    if (length(empty_cols) > 0) {
        warning(sprintf(
            "Empty columns may cause errors - detected empty columns: %s",
            paste(empty_cols, collapse = ", ")
        ))
    }

    # Coerce factorization
    is_factorizable_list <- function(l) {
        if (!is.list(l)) return(FALSE)
        all(sapply(l, function(x) is.logical(x) | is.character(x)))
    }
    is_factorizable <- function(obj) {
        is.logical(obj) | is.character(obj) | is_factorizable_list(obj)
    }

    if (is_factorizable(df[[variable_col_name]]))
        df[[variable_col_name]] <- factor(df[[variable_col_name]])
    df[[cohort_col_name]] <- factor(df[[cohort_col_name]])

    # Extract levels
    variable_levels <- levels(df[[variable_col_name]])
    cohort_levels <- levels(df[[cohort_col_name]])

    # Create the crosstab
    if (is.list(df[[variable_col_name]]))
        ct_sub <- new_crosstab_sub_multi(df, variable_col_name, cohort_col_name, variable_levels, cohort_levels)
    else
        ct_sub <- new_crosstab_sub(df, variable_col_name, cohort_col_name, variable_levels, cohort_levels)

    # Validate the crosstab was created with proper data
    validate_crosstab_sub(ct_sub)

    return(ct_sub)
}

# GETTERS
var_name <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    attr(ct_sub, "variable")
}

var <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    ct_sub[[var_name(ct_sub)]]
}

var_levels <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    attr(ct_sub, "variable_levels")
}

cohort_name <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    attr(ct_sub, "cohort")
}

cohort <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    ct_sub[[cohort_name(ct_sub)]]
}

cohort_levels <- function(ct_sub) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    attr(ct_sub, "cohort_levels")
}

# SETTERS
`var_name<-` <- function(ct_sub, value) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    names(ct_sub)[names(ct_sub) == var_name(ct_sub)] <- value
    attr(ct_sub, "variable") <- value
    return(ct_sub)
}

`var<-` <- function(ct_sub, value) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    ct_sub[[var_name(ct_sub)]] <- value
    return(ct_sub)
}

`var_levels<-` <- function(ct_sub, value) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    assert_that(!is.null(var_levels(ct_sub)), msg = sprintf(
        "Can not set factor levels for data of type %s",
        typeof(var(ct_sub))
    ))
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(setequal(var_levels(ct_sub), value), msg = "Provided levels must contain the same set of values as variable_levels")
    levels(var(ct_sub)) <- value
    attr(ct_sub, "variable_levels") <- value
    return(ct_sub)
}

`cohort_name<-` <- function(ct_sub, value) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    names(ct_sub)[names(ct_sub) == cohort_name(ct_sub)] <- value
    attr(ct_sub, "cohort") <- value
    return(ct_sub)
}

`cohort<-` <- function(ct_sub, value) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    ct_sub[[cohort_name(ct_sub)]] <- value
    return(ct_sub)
}

`cohort_levels<-` <- function(ct_sub, value) {
    assert_that(inherits(ct_sub, CT_SUB_CLASS))
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(setequal(cohort_levels(ct_sub), value), msg = "Provided levels must contain the same set of values as cohort_levels")
    levels(cohort(ct_sub)) <- value
    attr(ct_sub, "cohort_levels") <- value
    return(ct_sub)
}
