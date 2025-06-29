#' @import assertthat

# CONSTANTS
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

# CONSTRUCTORS
new_crosstab_data <- function(df, var_col_name, cohort_col_name, cohort_levels, var_levels = NULL, var_mapping = NULL, subclass = NULL) {
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

    classes <- c(subclass, CT_DATA_CLASS, class(df))

    structure(
        df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        var_mapping = var_mapping,
        class = classes
    )
}

new_crosstab_data_cat <- function(df, var_col_name, var_levels, cohort_col_name, cohort_levels) {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_CAT
    )
}

new_crosstab_data_num <- function(df, var_col_name, cohort_col_name, cohort_levels) {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_NUM
    )
}

new_crosstab_data_likert <- function(df, var_col_name, var_levels, var_mapping, cohort_col_name, cohort_levels) {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        var_mapping = var_mapping,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_LIKERT
    )
}

new_crosstab_data_multi <- function(df, var_col_name, var_levels, cohort_col_name, cohort_levels) {
    new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        subclass = CT_DATA_CLASS_MULTI
    )
}

# VALIDATORS
validate_crosstab_data_base <- function(ct_data) {
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

    assert_that(is.factor(cohort(ct_data)), msg = sprintf(
        "Cohort column \"%s\" must be a factor"
    ))
    assert_that(has_attr(ct_data, "cohort_levels"))
    cohort_levels <- cohort_levels(ct_data)
    assert_that(all(cohort(ct_data) %in% c(cohort_levels, NA)), msg = sprintf(
        "All values in %s must exist in cohort_levels",
        cohort_col_name
    ))

    return(TRUE)
}

validate_crosstab_data_cat <- function(ct_data) {
    validate_crosstab_data_base(ct_data)

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

validate_crosstab_data_num <- function(ct_data) {
    validate_crosstab_data_base(ct_data)

    assert_that(inherits(ct_data, CT_DATA_CLASS_NUM))
    assert_that(is.numeric(var(ct_data)), msg = "Data for this class must be numeric")

    return(TRUE)
}

validate_crosstab_data_likert <- function(ct_data) {
    validate_crosstab_data_base(ct_data)

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
            paste(setdiff(var(ct_data), names(var_mapping), NA), collapse = ", ")
        )
    )

    if (any(is.na(value))) warning("Certain values are mapped to NA")

    return(TRUE)
}

validate_crosstab_data_multi <- function(ct_data) {
    validate_crosstab_data_base(ct_data)

    assert_that(inherits(ct_data, CT_DATA_CLASS_MULTI))

    # assert_that(
    #     is.list(var(ct_data)),
    #
    # )
}

# HELPERS

# GETTERS
var_name <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    attr(ct_data, "var_col_name")
}

var <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    ct_data[[var_name(ct_data)]]
}

var_levels <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    attr(ct_data, "var_levels")
}

cohort_name <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    attr(ct_data, "cohort_col_name")
}

cohort <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    ct_data[[cohort_name(ct_data)]]
}

cohort_levels <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    attr(ct_data, "cohort_levels")
}

var_mapping <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    assert_that(inherits(ct_data, CT_DATA_CLASS_LIKERT))
    attr(ct_data, "var_mapping")
}

var_mapped <- function(ct_data) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    assert_that(inherits(ct_data, CT_DATA_CLASS_LIKERT))
    var_mapping(ct_data)[var(ct_data)]
}

# SETTERS
`var_name<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    names(ct_data)[names(ct_data) == var_name(ct_data)] <- value
    attr(ct_data, "var_col_name") <- value
    return(ct_data)
}

`var<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
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

`var_levels<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
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

`cohort_name<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    names(ct_data)[names(ct_data) == cohort_name(ct_data)] <- value
    attr(ct_data, "cohort_col_name") <- value
    return(ct_data)
}

`cohort<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    assert_that(is.factor(value))
    cohort_levels(ct_data) <- levels(value)
    ct_data[[cohort_name(ct_data)]] <- value
    return(ct_data)
}

`cohort_levels<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    assert_that(!any(duplicated(value)), msg = "Detected repeated values - factor levels must be unique")
    assert_that(setequal(cohort_levels(ct_data), value), msg = "Provided levels must contain the same set of values as cohort_levels")
    levels(ct_data[[cohort_name(ct_data)]]) <- value
    attr(ct_data, "cohort_levels") <- value
    return(ct_data)
}

`var_mapping<-` <- function(ct_data, value) {
    assert_that(inherits(ct_data, CT_DATA_CLASS))
    assert_that(inherits(ct_data, CT_DATA_CLASS_LIKERT))
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
    if (any(is.na(value))) warning("Certain values are mapped to NA")
    attr(ct_data, "var_mapping") <- value
    return(ct_data)
}
