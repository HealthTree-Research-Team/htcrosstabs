# CONSTRUCTORS ####
new_crosstab_data <- function(df, var_col_name, cohort_col_name, cohort_levels, var_levels = NULL, var_map = NULL, subclass = NULL, grouped = FALSE, combined_cohort_name = "All", desc_col_name = "Description") {
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
#' Create the Inner Sub-Table for a Crosstab Object
#'
#' You will likely never need to use the `crosstab_data()` constructor. It takes
#' a data frame and turns it into a structured, formatted table with attributes
#' helpful for htcrosstabs's internal functions. Under the hood, this function
#' is called during the broader call to the [crosstab()] constructor.
#'
#' @param df The data frame to be converted to a crosstab
#' @param cohort_col_name If applicable, the name of the column with the cohorts to split the data on.
#' @param var_map A named numeric vector (or list of vectors) for Likert-like data mapping categorical values to numeric values
#' @param new_var_col_name (Optional) You can rename the variable column with this parameter.
#' @param combined_cohort_name (Optional) The name of the "combined" column for grouped data.
#' @param desc_col_name (Optional) The name of the left-most description column in the output table.
#'
#' @returns A crosstab_data object, used for storing hidden data useful for internal functions.
#' @export
#'
#' @examples
#' num_ct <- crosstab_data(
#'     length_by_species,
#'     cohort_col_name = "species"
#' )
#' utils::tail(num_ct, 10)
#' names(attributes(num_ct))
#'
crosstab_data <- function(df, cohort_col_name = NULL, var_map = NULL, new_var_col_name = NULL, combined_cohort_name = "All", desc_col_name = "Description") {

    if (is.atomic(df)) {
        df <- data.frame(variable = df)
    } else if (is.list(df) & !is.data.frame(df)) {
        df <- data.frame(variable = I(df))
    }

    validate_input_crosstab_data(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name, new_var_col_name)
    grouped <- !is.null(cohort_col_name)

    # Add grouping column if it doesn't exist
    if (!grouped) {
        cohort_col_name <- get_non_matching("default_cohort", names(df))
        df[[cohort_col_name]] <- combined_cohort_name
    }

    clean_df <- function(df, cohort_col_name) {
        var_col_name <- names(df)[names(df) != cohort_col_name]
        if (!is.list(df[[var_col_name]]))
            return(df)

        islist <- FALSE
        if (is.factorlist(df[[var_col_name]])) {
            islist <- TRUE
            cur_levels <- levels(df[[var_col_name]])
        }

        df[[var_col_name]] <- lapply(df[[var_col_name]], function(x) {
            if (length(x) == 0)
                return(NA)
            else
                return(x)
        })

        if (islist) {
            df[[var_col_name]] <- factor(df[[var_col_name]], levels = cur_levels, drop_levels = TRUE)
        }

        return(df)
    }

    df <- clean_df(df, cohort_col_name)

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
#' @noRd
#' @export
var_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "var_col_name")
}

#' @noRd
#' @export
var.crosstab_data <- function(ct_data, raw = FALSE) {
    if (raw) ct_data <- get_raw_data(ct_data)
    ct_data[[var_name(ct_data)]]
}

#' @noRd
#' @export
var_levels.crosstab_data <- function(ct_data) {
    validate_input_var_levels_getter(ct_data)
    attr(ct_data, "var_levels")
}

#' @noRd
#' @export
cohort_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "cohort_col_name")
}

#' @noRd
#' @export
cohort.crosstab_data <- function(ct_data, raw = FALSE) {
    if (raw) ct_data <- get_raw_data(ct_data)
    ct_data[[cohort_name(ct_data)]]
}

#' @noRd
#' @export
cohort_levels.crosstab_data <- function(ct_data, raw = FALSE) {
    if (raw) ct_data <- get_raw_data(ct_data)
    attr(ct_data, "cohort_levels")
}

#' @noRd
#' @export
var_map.crosstab_data_likert <- function(ct_data) {
    attr(ct_data, "var_map")
}

#' @noRd
#' @export
var_mapped.crosstab_data_likert <- function(ct_data, raw = FALSE) {
    if (raw) ct_data <- get_raw_data(ct_data)
    var_map(ct_data)[as.character(var(ct_data))]
}

#' @noRd
#' @export
combined_cohort_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "combined_cohort_name")
}

#' @noRd
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
    raw_data[[cohort_name]] <- factor(raw_data[[cohort_name]], levels = new_levels, drop_levels = TRUE)

    # Change cohort levels attribute
    attr(raw_data, "cohort_levels") <- new_levels
    return(raw_data)
}

#' @noRd
#' @export
get_raw_data.crosstab_data <- function(ct_data) {
    return(ct_data)
}

#' @noRd
#' @export
desc_name.crosstab_data <- function(ct_data) {
    attr(ct_data, "desc_col_name")
}

# SETTERS ####
#' @noRd
#' @export
`var_name<-.crosstab_data` <- function(ct_data, value) {
    names(ct_data)[names(ct_data) == var_name(ct_data)] <- value
    attr(ct_data, "var_col_name") <- value
    return(ct_data)
}

#' @noRd
#' @export
`var<-.crosstab_data` <- function(ct_data, value) {
    validate_input_var_setter(ct_data, value)
    var_levels(ct_data) <- levels(value)
    ct_data[[var_name(ct_data)]] <- value
    return(ct_data)
}

#' @noRd
#' @export
`var_levels<-.crosstab_data` <- function(ct_data, value) {
    validate_input_var_levels_setter(ct_data, value)
    levels(ct_data[[var_name(ct_data)]]) <- value
    attr(ct_data, "var_levels") <- value
    return(ct_data)
}

#' @noRd
#' @export
`cohort_name<-.crosstab_data` <- function(ct_data, value) {
    names(ct_data)[names(ct_data) == cohort_name(ct_data)] <- value
    attr(ct_data, "cohort_col_name") <- value
    return(ct_data)
}

#' @noRd
#' @export
`cohort<-.crosstab_data` <- function(ct_data, value) {
    validate_input_cohort_setter(ct_data, value)
    ct_data[[cohort_name(ct_data)]] <- value
    cohort_levels(ct_data) <- levels(value)
    return(ct_data)
}

#' @noRd
#' @export
`cohort_levels<-.crosstab_data` <- function(ct_data, value) {
    validate_input_cohort_levels_setter(ct_data, value)
    levels(ct_data[[cohort_name(ct_data)]]) <- value
    attr(ct_data, "cohort_levels") <- value
    return(ct_data)
}

#' @noRd
#' @export
`var_map<-.crosstab_data_likert` <- function(ct_data, value) {
    validate_input_var_map_setter(ct_data, value)
    attr(ct_data, "var_map") <- value
    return(ct_data)
}
