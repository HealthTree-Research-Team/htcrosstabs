# CONSTRUCTORS ####
new_crosstab_data <- function(df, var_col_name, cohort_col_name, cohort_levels, var_levels = NULL, var_map = NULL, subclass = NULL, grouped = F, combined_cohort_name = "All", desc_col_name = "Description") {
    validate_input_new_crosstab_data(df, var_col_name, cohort_col_name, cohort_levels, var_levels, var_map, subclass, grouped, combined_cohort_name, desc_col_name)

    combined_cohort_name <- if (grouped) combined_cohort_name else NULL
    grouping_class <- if (grouped) CT_DATA_CLASS_GROUPED else NULL
    classes <- c(subclass, grouping_class, CT_DATA_CLASS, class(df))

    structure(
        df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        combined_cohort_name = combined_cohort_name,
        var_map = var_map,
        desc_col_name = desc_col_name,
        class = classes
    )
}

# HELPERS ####
#' @export
crosstab_data <- function(df, cohort_col_name = NULL, var_map = NULL, new_var_col_name = NULL, combined_cohort_name = "All", desc_col_name = "Description") {
    validate_input_crosstab_data(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name, new_var_col_name)
    grouped <- !is.null(cohort_col_name)

    # Add grouping column if it doesn't exist
    if (!grouped) {
        cohort_col_name <- get_non_matching("cohort", names(df))
        df[[cohort_col_name]] <- combined_cohort_name
    }

    # Factorize cohort and variable (if non-numeric)
    df[[cohort_col_name]] <- factor(df[[cohort_col_name]])
    var_col_name <- names(df)[names(df) != cohort_col_name]
    if (!is.null(new_var_col_name)) {
        names(df)[names(df) == var_col_name] <- new_var_col_name
        var_col_name <- new_var_col_name
    }
    if (!is.numeric(df[[var_col_name]])) {
        df[[var_col_name]] <- factor(df[[var_col_name]])
    }

    # Extract levels (var_levels will be NULL for numeric variables)
    var_levels <- levels(df[[var_col_name]])
    cohort_levels <- levels(df[[cohort_col_name]])

    # Add the "All" group if it's grouped
    if (grouped) {
        cohort_levels <- c(combined_cohort_name, cohort_levels)
        all_df <- df
        all_df[[cohort_col_name]] <- combined_cohort_name
        df <- rbind(all_df, df)
        df[[cohort_col_name]] <- factor(df[[cohort_col_name]], levels = cohort_levels)
    }

    # Determine subclass
    subclass <- determine_col_type(df[[var_col_name]], var_map = var_map)

    # Call the right constructor
    ct_data <- new_crosstab_data(
        df = df,
        var_col_name = var_col_name,
        var_levels = var_levels,
        var_map = var_map,
        cohort_col_name = cohort_col_name,
        cohort_levels = cohort_levels,
        grouped = grouped,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name,
        subclass = subclass
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
var.crosstab_data <- function(ct_data, raw = F) {
    if (raw) ct_data <- get_raw_data(ct_data)
    ct_data[[var_name(ct_data)]]
}

#' @export
var_levels.crosstab_data <- function(ct_data) {
    validate_input_var_levels_getter(ct_data)
    attr(ct_data, "var_levels")
}

#' @export
cohort_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "cohort_col_name")
}

#' @export
cohort.crosstab_data <- function(ct_data, raw = F) {
    if (raw) ct_data <- get_raw_data(ct_data)
    ct_data[[cohort_name(ct_data)]]
}

#' @export
cohort_levels.crosstab_data <- function(ct_data, raw = F) {
    if (raw) ct_data <- get_raw_data(ct_data)
    attr(ct_data, "cohort_levels")
}

#' @export
var_map.crosstab_data_likert <- function(ct_data) {
    attr(ct_data, "var_map")
}

#' @export
var_mapped.crosstab_data_likert <- function(ct_data, raw = F) {
    if (raw) ct_data <- get_raw_data(ct_data)
    var_map(ct_data)[as.character(var(ct_data))]
}

#' @export
combined_cohort_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "combined_cohort_name")
}

#' @export
get_raw_data.crosstab_data_grouped <- function(ct_data) {
    cohort_vals <- cohort(ct_data)
    all_cohort <- combined_cohort_name(ct_data)
    keep <- !(cohort_vals %in% all_cohort)
    raw_data <- ct_data[keep, , drop = FALSE]
    rownames(raw_data) <- NULL

    # Convert factor levels
    cohort_name <- cohort_name(raw_data)
    new_levels <- setdiff(levels(raw_data[[cohort_name]]), all_cohort)
    raw_data[[cohort_name]] <- factor(raw_data[[cohort_name]], levels = new_levels, drop_levels = T)

    # Change cohort levels attribute
    attr(raw_data, "cohort_levels") <- new_levels
    return(raw_data)
}

#' @export
get_raw_data.crosstab_data <- function(ct_data) {
    return(ct_data)
}

#' @export
desc_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "desc_col_name")
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
    validate_input_var_setter(ct_data, value)
    var_levels(ct_data) <- levels(value)
    ct_data[[var_name(ct_data)]] <- value
    return(ct_data)
}

#' @export
`var_levels<-.crosstab_data` <- function(ct_data, value) {
    validate_input_var_levels_setter(ct_data, value)
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
    validate_input_cohort_setter(ct_data, value)
    ct_data[[cohort_name(ct_data)]] <- value
    cohort_levels(ct_data) <- levels(value)
    return(ct_data)
}

#' @export
`cohort_levels<-.crosstab_data` <- function(ct_data, value) {
    validate_input_cohort_levels_setter(ct_data, value)
    levels(ct_data[[cohort_name(ct_data)]]) <- value
    attr(ct_data, "cohort_levels") <- value
    return(ct_data)
}

#' @export
`var_map<-.crosstab_data_likert` <- function(ct_data, value) {
    validate_input_var_map_setter(ct_data, value)
    attr(ct_data, "var_map") <- value
    return(ct_data)
}
