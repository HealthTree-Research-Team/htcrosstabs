# IMPORTS ####
#' @import assertthat

# CONSTANTS ####
CT_DATA_CLASS <- "crosstab_data"
CT_DATA_CLASS_CAT <- "crosstab_data_cat"
CT_DATA_CLASS_NUM <- "crosstab_data_num"
CT_DATA_CLASS_LIKERT <- "crosstab_data_likert"
CT_DATA_CLASS_MULTI <- "crosstab_data_multi"
CT_DATA_CLASSES <- c(
    CT_DATA_CLASS,
    CT_DATA_CLASS_CAT,
    CT_DATA_CLASS_NUM,
    CT_DATA_CLASS_LIKERT,
    CT_DATA_CLASS_MULTI
)
CT_DATA_CLASS_GROUPED <- "crosstab_data_grouped"

# CONSTRUCTORS ####
new_crosstab_data <- function(df, var_col_name, cohort_col_name, cohort_levels, var_levels = NULL, var_mapping = NULL, subclass = NULL, grouped = F, all_cohort_name = "All") {
    assert_that(is.data.frame(df))
    assert_that(is.character(var_col_name))
    assert_that(length(var_col_name) == 1, msg = "var_col_name must only have one value")
    assert_that(is.character(cohort_col_name))
    assert_that(length(cohort_col_name) == 1, msg = "cohort_col_name must only have one value")
    assert_that(is.character(cohort_levels))

    if (!is.null(var_levels))
        assert_that(is.character(var_levels))

    if (!is.null(var_mapping))
        assert_that(
            is.numeric(var_mapping),
            !is.null(names(var_mapping)),
            msg = "var_mapping must be a named vector of numeric values"
        )

    if (!is.null(subclass))
        assert_that(is.character(subclass))

    grouping_class <- if (grouped) CT_DATA_CLASS_GROUPED else NULL

    all_cohort_name <- if (grouped) all_cohort_name else NULL

    classes <- c(subclass, grouping_class, CT_DATA_CLASS, class(df))

    structure(
        df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        all_cohort_name = all_cohort_name,
        var_mapping = var_mapping,
        class = classes
    )
}

new_crosstab_data_cat <- function(df, var_col_name, var_levels, cohort_col_name, cohort_levels, grouped = F, all_cohort_name = "All") {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_CAT,
        grouped = grouped,
        all_cohort_name = all_cohort_name
    )
}

new_crosstab_data_num <- function(df, var_col_name, cohort_col_name, cohort_levels, grouped = F, all_cohort_name = "All") {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_NUM,
        grouped = grouped,
        all_cohort_name = all_cohort_name
    )
}

new_crosstab_data_likert <- function(df, var_col_name, var_levels, var_mapping, cohort_col_name, cohort_levels, grouped = F, all_cohort_name = "All") {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        var_mapping = var_mapping,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_LIKERT,
        grouped = grouped,
        all_cohort_name = all_cohort_name
    )
}

new_crosstab_data_multi <- function(df, var_col_name, var_levels, cohort_col_name, cohort_levels, grouped = F, all_cohort_name = "All") {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_MULTI,
        grouped = grouped,
        all_cohort_name = all_cohort_name
    )
}

# VALIDATORS ####
#' @export
validate_crosstab_data.crosstab_data <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
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

    if (has_attr(ct_data, "all_cohort_name")) {
        assert_that(is.character(all_cohort_name(ct_data)), msg = "Provided all_cohort_name must be a character")
    }

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

    return(TRUE)
}

#' @export
validate_crosstab_data.crosstab_data_cat <- function(ct_data) {
    validate_crosstab_data.crosstab_data(ct_data)

    assert_that(inherits(ct_data, CT_DATA_CLASS_CAT))
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

    assert_that(inherits(ct_data, CT_DATA_CLASS_NUM))
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

    assert_that(inherits(ct_data, CT_DATA_CLASS_LIKERT))
    assert_that(is.factor(var(ct_data)), msg = "Likert data must be a factor")

    assert_that(has_attr(ct_data, "var_levels"))
    var_levels <- var_levels(ct_data)
    assert_that(all(var(ct_data) %in% c(var_levels, NA)), msg = sprintf(
        "All values in %s must exist in var_levels",
        var_name(ct_data)
    ))

    assert_that(has_attr(ct_data, "var_mapping"))
    var_mapping <- var_mapping(ct_data)
    assert_that(
        is.numeric(var_mapping),
        !is.null(names(var_mapping)),
        msg = "var_mapping must be a named vector of numeric values"
    )
    assert_that(
        !any(duplicated(names(var_mapping))),
        msg = "Detected multiple values with the same name"
    )
    assert_that(
        all(var(ct_data) %in% c(names(var_mapping), NA)),
        msg = sprintf(
            "Detected unmapped values: %s",
            paste(setdiff(var(ct_data), c(names(var_mapping), NA)), collapse = ", ")
        )
    )

    return(TRUE)
}

#' @export
validate_crosstab_data.crosstab_data_multi <- function(ct_data) {
    validate_crosstab_data.crosstab_data(ct_data)

    assert_that(inherits(ct_data, CT_DATA_CLASS_MULTI))
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

# HELPERS ####
#' @export
crosstab_data <- function(df, cohort_col_name = NULL, likert_map = NULL, default_cohort = "All", all_cohort_name = "All") {
    assert_that(is.data.frame(df))

    # If it isn't grouped, add grouping column
    if (is.null(cohort_col_name)) {
        grouped <- F
        assert_that(ncol(df) == 1, msg = sprintf(
            "If cohort_col_name is left NULL df must only have 1 column for data - detected %d columns",
            ncol(df)
        ))

        # Create a cohort column name that doesn't already exist
        cohort_col_name = "cohort"
        while (cohort_col_name %in% names(df))
            cohort_col_name = paste0(cohort_col_name, "_autogenerated")

        df[[cohort_col_name]] <- factor(default_cohort)
    } else {
        grouped <- T
        assert_that(ncol(df) == 2, msg = sprintf(
            "If providing cohort_col_name df must have 2 columns: one for data and one for cohort"
        ))
        assert_that(cohort_col_name %in% names(df), msg = sprintf(
            "Cohort column \"%s\" not found in df",
            cohort_col_name
        ))
        assert_that(!is.list(df[[cohort_col_name]]), msg = "Cohort column cannot be a list")
    }

    var_col_name <- names(df)[names(df) != cohort_col_name]

    # Warn if there are empty columns
    empty_cols <- names(df)[sapply(df, function(x) all(is.na(x)))]
    if (length(empty_cols) > 0) {
        warning(sprintf(
            "Empty columns may cause errors - detected empty columns: %s",
            paste(empty_cols, collapse = ", ")
        ))
    }

    if (!is.numeric(df[[var_col_name]])) {
        if (!is.factor(df[[var_col_name]]) & !is.factorlist(df[[var_col_name]]))
            warning(sprintf(
                "Coercing variable column to %s",
                if (is.list(df[[var_col_name]])) "factor list" else "factor"
            ))
        df[[var_col_name]] <- factor(df[[var_col_name]])
    }

    if (!is.factor(df[[cohort_col_name]]))
        warning("Coercing cohort column to factor")
    df[[cohort_col_name]] <- factor(df[[cohort_col_name]])

    # Extract levels (bear in mind, if provided a
    # numeric variable, var_levels will be NULL)
    var_levels <- levels(df[[var_col_name]])
    cohort_levels <- levels(df[[cohort_col_name]])

    # Add the "All" group if it's grouped
    if (grouped) {
        assert_that(
            !(all_cohort_name %in% cohort_levels),
            msg = "If \"All\" is already one of the cohort names, assign a different default \"All\" cohort name with all_cohort_name = [name]"
        )
        cohort_levels <- c(all_cohort_name, cohort_levels)
        all_df <- df
        all_df[[cohort_col_name]] <- all_cohort_name
        df <- rbind(all_df, df)
        df[[cohort_col_name]] <- factor(df[[cohort_col_name]])
    }

    # Call the right constructor
    if (is.list(df[[var_col_name]]))
        ct_data <- new_crosstab_data_multi(
            df = df,
            var_col_name = var_col_name,
            var_levels = var_levels,
            cohort_col_name = cohort_col_name,
            cohort_levels = cohort_levels,
            grouped = grouped,
            all_cohort_name = all_cohort_name
        )
    else if (is.numeric(df[[var_col_name]]))
        ct_data <- new_crosstab_data_num(
            df = df,
            var_col_name = var_col_name,
            cohort_col_name = cohort_col_name,
            cohort_levels = cohort_levels,
            grouped = grouped,
            all_cohort_name = all_cohort_name
        )
    else if (!is.null(likert_map))
        ct_data <- new_crosstab_data_likert(
            df = df,
            var_col_name = var_col_name,
            var_levels = var_levels,
            var_mapping = likert_map,
            cohort_col_name = cohort_col_name,
            cohort_levels = cohort_levels,
            grouped = grouped,
            all_cohort_name = all_cohort_name
        )
    else
        ct_data <- new_crosstab_data_cat(
            df = df,
            var_col_name = var_col_name,
            var_levels = var_levels,
            cohort_col_name = cohort_col_name,
            cohort_levels = cohort_levels,
            grouped = grouped,
            all_cohort_name = all_cohort_name
        )

    # Validate
    validate_crosstab_data(ct_data)

    return(ct_data)
}

# GETTERS ####
#' @export
var_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "var_col_name")
}

#' @export
var.crosstab_data <- function(ct_data) {
    ct_data[[var_name(ct_data)]]
}

#' @export
var_levels.crosstab_data <- function(ct_data) {
    assert_that(
        !inherits(ct_data, CT_DATA_CLASS_NUM),
        msg = sprintf(
            "Can not call var_levels on object of type %s",
            CT_DATA_CLASS_NUM
        )
    )
    attr(ct_data, "var_levels")
}

#' @export
cohort_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "cohort_col_name")
}

#' @export
cohort.crosstab_data <- function(ct_data) {
    ct_data[[cohort_name(ct_data)]]
}

#' @export
cohort_levels.crosstab_data <- function(ct_data) {
    attr(ct_data, "cohort_levels")
}

#' @export
var_mapping.crosstab_data_likert <- function(ct_data) {
    attr(ct_data, "var_mapping")
}

#' @export
var_mapped.crosstab_data_likert <- function(ct_data, all = T) {
    if (!all) ct_data <- get_raw_data(ct_data)
    var_mapping(ct_data)[var(ct_data)]
}

#' @export
data_mapped.crosstab_data_likert <- function(ct_data) {
    mapped_ct_data <- ct_data

    # Manually create a numeric crosstab_data object
    mapped_ct_data[[var_name(ct_data)]] <- var_mapped(ct_data)
    attr(mapped_ct_data, "var_levels") <- NULL
    attr(mapped_ct_data, "var_mapping") <- NULL
    class(mapped_ct_data)[class(mapped_ct_data) == CT_DATA_CLASS_LIKERT] <- CT_DATA_CLASS_NUM

    # Double check you created a valid object
    validate_crosstab_data(mapped_ct_data)

    return(mapped_ct_data)
}

#' @export
is_grouped.crosstab_data <- function(ct_data) {
    inherits(ct_data, CT_DATA_CLASS_GROUPED)
}

#' @export
all_cohort_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "all_cohort_name")
}

#' @export
get_raw_data.crosstab_data_grouped <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    assert_that(has_attr(ct_data, "all_cohort_name"))

    cohort_vals <- cohort(ct_data)
    all_cohort <- all_cohort_name(ct_data)

    keep <- !(cohort_vals %in% all_cohort)
    result <- ct_data[keep, , drop = FALSE]
    rownames(result) <- NULL

    return(result)
}

#' @export
get_raw_data.crosstab_data <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))

    return(ct_data)
}

# SETTERS ####
#' @export
`var_name<-.crosstab_data` <- function(ct_data, value) {
    names(ct_data)[names(ct_data) == var_name(ct_data)] <- value
    attr(ct_data, "var_col_name") <- value
    return(ct_data)
}

#' @export
`var<-.crosstab_data` <- function(ct_data, value) {
    assert_that(
        are_equal(
            typeof(ct_data[[var_name(ct_data)]]),
            typeof(value)
        ),
        msg = "Type mismatch - new data must be the same type as old data"
    )
    var_levels(ct_data) <- levels(value)
    ct_data[[var_name(ct_data)]] <- value
    return(ct_data)
}

#' @export
`var_levels<-.crosstab_data` <- function(ct_data, value) {
    assert_that(!is.null(var_levels(ct_data)), msg = sprintf(
        "Can not set factor levels - data type: %s, data object class: %s",
        typeof(var(ct_data)),
        paste(intersect(class(ct_data), CT_DATA_CLASSES), collapse = ", ")
    ))
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(setequal(var_levels(ct_data), value), msg = "Provided levels must contain the same set of values as var_levels")
    levels(ct_data[[var_name(ct_data)]]) <- value
    attr(ct_data, "var_levels") <- value
    return(ct_data)
}

#' @export
`cohort_name<-.crosstab_data` <- function(ct_data, value) {
    names(ct_data)[names(ct_data) == cohort_name(ct_data)] <- value
    attr(ct_data, "cohort_col_name") <- value
    return(ct_data)
}

#' @export
`cohort<-.crosstab_data` <- function(ct_data, value) {
    assert_that(is.factor(value))
    ct_data[[cohort_name(ct_data)]] <- value
    cohort_levels(ct_data) <- levels(value)
    return(ct_data)
}

#' @export
`cohort_levels<-.crosstab_data` <- function(ct_data, value) {
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(all(ct_data[[cohort_name(ct_data)]] %in% c(value, NA)), msg = "Detected values in cohort column not in new levels")
    levels(ct_data[[cohort_name(ct_data)]]) <- value
    attr(ct_data, "cohort_levels") <- value
    return(ct_data)
}

#' @export
`var_mapping<-.crosstab_data_likert` <- function(ct_data, value) {
    assert_that(
        is.numeric(value),
        !is.null(names(value)),
        msg = "var_mapping must be a named vector of numeric values"
    )
    assert_that(!any(duplicated(names(value))), msg = "Detected multiple values with the same name")
    assert_that(
        setequal(names(value), names(var_mapping(ct_data))),
        msg = "New mapping must contain same names as previous mapping"
    )
    attr(ct_data, "var_mapping") <- value
    return(ct_data)
}
