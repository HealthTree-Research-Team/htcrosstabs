# IMPORTS ####
#' @import assertthat

# VALIDATE CONSTRUCTORS ####
validate_input_new_crosstab_data <- function(df, var_col_name, cohort_col_name, cohort_levels, var_levels, var_map, subclass, grouped, combined_cohort_name, desc_col_name) {
    assert_that(is.data.frame(df))
    assert_that(is.character(var_col_name))
    assert_that(length(var_col_name) == 1, msg = "var_col_name must only have one value")
    assert_that(is.character(cohort_col_name))
    assert_that(length(cohort_col_name) == 1, msg = "cohort_col_name must only have one value")
    assert_that(is.character(cohort_levels))
    assert_that(is.logical(grouped))
    assert_that(is.character(combined_cohort_name))
    assert_that(is.character(desc_col_name))
    if (!is.null(var_levels))
        assert_that(is.character(var_levels))
    if (!is.null(var_map))
        assert_that(
            is.numeric(var_map),
            !is.null(names(var_map)),
            msg = "var_map must be a named vector of numeric values"
        )
    if (!is.null(subclass))
        assert_that(is.character(subclass))
}

validate_input_crosstab_data <- function(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name, new_var_col_name = NULL) {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name))
        assert_that(is.character(cohort_col_name))
    if (!is.null(var_map))
        assert_that(
            is.numeric(var_map),
            !is.null(names(var_map)),
            msg = "var_map must be a named vector of numeric values"
        )
    assert_that(is.character(combined_cohort_name))
    assert_that(is.character(desc_col_name))

    # Warn if there are empty columns
    empty_cols <- names(df)[sapply(df, function(x) all(is.na(x)))]
    if (length(empty_cols) > 0) {
        warning(sprintf(
            "Detected empty columns - may cause errors: %s",
            paste(empty_cols, collapse = ", ")
        ))
    }

    if (!is.null(cohort_col_name)) {
        assert_that(ncol(df) == 2, msg = sprintf(
            "If providing cohort_col_name df must have 2 columns: one for data and one for cohort"
        ))
        assert_that(cohort_col_name %in% names(df), msg = sprintf(
            "Cohort column \"%s\" not found in df",
            cohort_col_name
        ))
        assert_that(!is.list(df[[cohort_col_name]]), msg = "Cohort column cannot be a list")
        assert_that(!(combined_cohort_name %in% df[[cohort_col_name]]), msg = sprintf(
            "%s already in cohort column, please specify a different combined_cohort_name",
            combined_cohort_name
        ))
    } else {
        if (ncol(df) == 2)
            error_msg <- "When providing 2 columns, you must specify cohort_col_name"
        else
            error_msg <- sprintf(
                "If cohort_col_name is left NULL df must only have 1 column for data - detected %d columns",
                ncol(df)
            )
        assert_that(ncol(df) == 1, msg = error_msg)
    }

    if (!is.null(new_var_col_name)) {
        assert_that(is.character(new_var_col_name))
        if (!is.null(cohort_col_name)) {
            assert_that(new_var_col_name != cohort_col_name, msg = "Can not rename variable column to the same name as the cohort column")
        }
    }
}

#' @export
validate_crosstab_data.crosstab_data <- function(ct_data) {
    assert_crosstab_data(ct_data)
    assert_that(ncol(ct_data) == 2, msg = "df must have 2 columns: one for variable and one for cohort")

    assert_that(has_attr(ct_data, "var_col_name"))
    var_col_name <- var_name(ct_data)
    assert_that(var_col_name %in% names(ct_data), msg = sprintf(
        "%s column not found in df",
        var_col_name
    ))

    assert_that(has_attr(ct_data, "cohort_col_name"))
    cohort_col_name <- cohort_name(ct_data)
    assert_that(cohort_col_name %in% names(ct_data), msg = sprintf(
        "%s column not found in df",
        cohort_col_name
    ))

    if (has_attr(ct_data, "combined_cohort_name")) {
        assert_that(is.character(combined_cohort_name(ct_data)), msg = "Provided combined_cohort_name must be a character")
    }

    assert_that(!any(is.na(cohort(ct_data))), msg = sprintf(
        "Detected NA values in cohort column \"%s\", may cause unpredictable behavior. If you wish to keep NA values, it is preferred that you use a placeholder value like \"NA\" or \"Unknown\".",
        cohort_col_name
    ))

    assert_that(!is.list(cohort(ct_data)), msg = sprintf(
        "Cohort column \"%s\" can not be a list",
        cohort_col_name
    ))
    assert_that(is.factor(cohort(ct_data)), msg = sprintf(
        "Cohort column \"%s\" must be a factor",
        cohort_col_name
    ))
    assert_that(has_attr(ct_data, "cohort_levels"))
    cohort_levels <- cohort_levels(ct_data)
    assert_that(all(cohort(ct_data) %in% c(cohort_levels, NA)), msg = sprintf(
        "All values in %s must exist in cohort_levels",
        cohort_col_name
    ))

    assert_that(has_attr(ct_data, "desc_col_name"))
    desc_col_name <- desc_name(ct_data)
    assert_that(
        !any(duplicated(c(var_col_name, cohort_col_name, desc_col_name))),
        msg = paste(
            "desc_col_name must not match var_col_name or cohort_col_name,",
            "please choose a unique name for the description column with desc_col_name = [name]",
            collapse = "\n"
        )
    )

    return(TRUE)
}

#' @export
validate_crosstab_data.crosstab_data_cat <- function(ct_data) {
    validate_crosstab_data.crosstab_data(ct_data)

    assert_crosstab_categorical(ct_data)
    assert_that(is.factor(var(ct_data)), msg = "Categorical data must be a factor")

    assert_that(has_attr(ct_data, "var_levels"))
    var_levels <- var_levels(ct_data)
    assert_that(all(var(ct_data) %in% c(var_levels, NA)), msg = sprintf(
        "All values in %s must exist in var_levels",
        var_name(ct_data)
    ))

    return(TRUE)
}

#' @export
validate_crosstab_data.crosstab_data_num <- function(ct_data) {
    validate_crosstab_data.crosstab_data(ct_data)

    assert_crosstab_numeric(ct_data)
    assert_that(is.numeric(var(ct_data)), msg = "Data for this class must be numeric")
    assert_that(
        !has_attr(ct_data, "var_levels"),
        msg = "Numeric data should not have var_levels attribute"
    )

    return(TRUE)
}

#' @export
validate_crosstab_data.crosstab_data_likert <- function(ct_data) {
    validate_crosstab_data.crosstab_data(ct_data)

    assert_crosstab_likert(ct_data)
    assert_that(is.factor(var(ct_data)), msg = "Likert data must be a factor")

    assert_that(has_attr(ct_data, "var_levels"))
    var_levels <- var_levels(ct_data)
    assert_that(all(var(ct_data) %in% c(var_levels, NA)), msg = sprintf(
        "All values in %s must exist in var_levels",
        var_name(ct_data)
    ))

    assert_that(has_attr(ct_data, "var_map"))
    var_map <- var_map(ct_data)
    assert_that(
        is.numeric(var_map),
        !is.null(names(var_map)),
        msg = "var_map must be a named vector of numeric values"
    )
    assert_that(
        !any(duplicated(names(var_map))),
        msg = "Detected multiple values with the same name"
    )
    assert_that(
        all(var(ct_data) %in% c(names(var_map), NA)),
        msg = sprintf(
            "Detected unmapped values: %s",
            paste(setdiff(var(ct_data), c(names(var_map), NA)), collapse = ", ")
        )
    )

    return(TRUE)
}

#' @export
validate_crosstab_data.crosstab_data_multi <- function(ct_data) {
    validate_crosstab_data.crosstab_data(ct_data)

    assert_crosstab_multi(ct_data)
    assert_that(is.factorlist(var(ct_data)), msg = "Variable column must be a list of factors (htcrosstabs::factor() can help)")

    assert_that(has_attr(ct_data, "var_levels"))
    var_levels <- var_levels(ct_data)
    assert_that(all(unlist(var(ct_data)) %in% c(var_levels, NA)), msg = sprintf(
        "All values in %s must exist in var_levels",
        var_name(ct_data)
    ))

    return(TRUE)
}

validate_crosstab_data <- function(ct_data) {
    UseMethod("validate_crosstab_data", ct_data)
}

# VALIDATE GETTERS ####
validate_input_var_levels_getter <- function(ct_data) {
    assert_that(!is.crosstab.numeric(ct_data), msg = sprintf(
        "Can not call var_levels on object of type %s",
        CT_DATA_CLASS_NUM
    ))
}

# VALIDATE SETTERS ####
validate_input_var_setter <- function(ct_data, value) {
    get_type <- function(obj) {
        if (is.list(obj)) "list"
        else if (is.factor(obj)) "factor"
        else if (is.numeric(obj)) "numeric"
        else typeof(obj)
    }
    assert_that(
        are_equal(get_type(value), get_type(ct_data[[var_name(ct_data)]])),
        msg = sprintf(
            "Type mismatch: new data must be the same type as old data, detected types %s and %s",
            get_type(value),
            get_type(ct_data[[var_name(ct_data)]])
        )
    )
}

validate_input_var_levels_setter <- function(ct_data, value) {
    assert_that(is.character(value))
    assert_that(!is.crosstab.numeric(ct_data), msg = "Can not set var_levels on numeric crosstab")
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(setequal(var_levels(ct_data), value), msg = "Provided levels must contain the same set of values as var_levels")
}

validate_input_cohort_setter <- function(ct_data, value) {
    assert_that(is.factor(value))
}

validate_input_cohort_levels_setter <- function(ct_data, value) {
    assert_that(is.character(value))
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(all(ct_data[[cohort_name(ct_data)]] %in% c(value, NA)), msg = "Detected values in cohort column not in new levels")
}

validate_input_var_map_setter <- function(ct_data, value) {
    assert_that(
        is.numeric(value),
        !is.null(names(value)),
        msg = "var_map must be a named vector of numeric values"
    )
    assert_that(!any(duplicated(names(value))), msg = "Detected multiple values with the same name")
    assert_that(
        setequal(names(value), names(var_map(ct_data))),
        msg = "New mapping must contain same names as previous mapping"
    )
}
