# CONSTRUCTORS ####
new_crosstab <- function(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name, new_var_col_name = NULL) {
    validate_input_new_crosstab(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name)

    if (is.crosstab.data(df)) {
        data <- df
    } else {
        data <- crosstab_data(
            df = df,
            cohort_col_name = cohort_col_name,
            var_map = var_map,
            new_var_col_name = new_var_col_name,
            combined_cohort_name = combined_cohort_name,
            desc_col_name = desc_col_name
        )
    }

    structure(
        data.frame(), # Output table
        data = data,  # crosstab_data
        index = character(0),
        table_id = numeric(0),
        table_name = character(0),
        table_type = character(0),
        footnotes = list(),
        manual_escape = FALSE,
        class = c(CT_CLASS, class(data.frame()))
    )
}

# HELPERS ####
#' Create a Crosstab Object
#'
#' `crosstab()` turns a data frame into a crosstab object, allowing for easy
#' calculation and output. The object itself is the output table, with extra
#' attributes storing different aspects of the data, accessible with the
#' `get_*()` functions.
#'
#' The crosstab object allows for easy calculation of certain variables and
#' output. The variables are accessible via the `get_*()` functions.
#'
#' The output table has a description column on the left, followed by a combined
#' or "All" column (if the data is grouped), followed by a column for each
#' cohort. This output table can be filled by calls to the `add_*_row()`
#' functions, or adding custom rows with calls to `add_rows()`.
#'
#' The order of variables, both in terms of rows and columns, are determined by
#' the factor levels of the variables and cohorts respectively. Factorizing
#' list-columns of multi-response data can be a hassle, so we have also provided
#' an override of the [factor()] and [levels()] functions.
#'
#' The `as.crosstab()` function simply passes the inputs on to the [crosstab()]
#' constructor (crosstab objects are returned as-is).
#'
#' @inheritParams crosstab_data
#'
#' @returns A completed crosstab object
#' @export
#'
#' @examples
#' # Ungrouped data
#' num_ct <- crosstab(petal_width)
#' add_default_table(num_ct)
#'
#' # Grouped data
#' num_ct <- crosstab(length_by_species, cohort_col_name = "species")
#' add_default_table(num_ct)
#'
crosstab <- function(df, cohort_col_name = NULL, var_map = NULL, new_var_col_name = NULL, combined_cohort_name = "All", desc_col_name = "Description") {
    if (is.crosstab(df))
        return(df)

    if (is.atomic(df)) {
        df <- data.frame(variable = df)
    } else if (is.list(df) & !is.data.frame(df)) {
        df <- data.frame(variable = I(df))
    }

    ct <- new_crosstab(
        df = df,
        cohort_col_name = cohort_col_name,
        var_map = var_map,
        new_var_col_name = new_var_col_name,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name
    )

    validate_crosstab(ct)

    if (!is.null(var_map))
        ct <- add_likert_map_footnotes(ct, var_map)

    return(ct)
}

#' @describeIn crosstab Another way to call `crosstab()`
#' @export
as.crosstab <- function(df, cohort_col_name = NULL, var_map = NULL, new_var_col_name = NULL, combined_cohort_name = "All", desc_col_name = "Description") {
    crosstab(df = df, cohort_col_name = cohort_col_name, var_map = var_map, new_var_col_name = new_var_col_name, combined_cohort_name = combined_cohort_name, desc_col_name = desc_col_name)
}

# GETTERS ####
#' Extract the Data the Crosstab Uses
#'
#' `data_table()` returns the crosstab_data object at the heart of the crosstab.
#' It stores the data from the data frame that was passed in. Since grouped
#' data adds a combined or "All" column in the output, That extra data is added
#' on. If you want the data without the combined or "All" cohort, set `raw = TRUE`.
#'
#' The setters `set_new_data()` and `data_table(ct) <- new_data` can be passed
#' either a crosstab_data object or another full crosstab object to copy the
#' data table from. The new data table doesn't have the be the same data type,
#' and any rows/tables added from then on will be based on the new data and
#' their corresponding index will be updated with the `var_name()` from the new
#' data.
#'
#' @param ct The crosstab object from which you wish to extract the current data table
#' @param value,tbl The replacement data table
#' @param var_map A named numeric vector in case `value` or `tbl` are a data frame of likert values
#' @param raw Logical, get rid of combined or "All" column? (Optional)
#'
#' @returns The crosstab_data object at the heart of the crosstab object.
#' @export
#'
#' @examples
#' # Ungrouped data
#' num_ct <- crosstab(petal_width)
#'
#' num_data_table <- data_table(num_ct)
#' head(num_data_table, 10) # A default cohort column is added to ungrouped data
#'
#' # Grouped data
#' num_df <- iris[, c("Sepal.Length", "Species"), drop = FALSE]
#' num_ct <- crosstab(num_df, cohort_col_name = "Species")
#'
#' num_data_table <- data_table(num_ct)
#' head(num_data_table, 10) # A combined cohort is added to grouped data
#'
#' raw_data_table <- data_table(num_ct, raw = TRUE)
#' head(raw_data_table, 10) # raw = TRUE removes the combined cohort
#'
#' # Setting new data
#' new_df <- iris[, c("Sepal.Width", "Species"), drop = FALSE]
#' new_data <- crosstab_data(new_df, cohort_col_name = "Species")
#' data_table(num_ct) <- new_data
#' head(data_table(num_ct, raw = TRUE), 10)
#'
data_table <- function(ct, raw = FALSE) {
    validate_input_data_table_getter(ct, raw)
    ct_data <- attr(ct, "data")
    if (raw) ct_data <- get_raw_data(ct_data)
    return(ct_data)
}

#' Create an Index of Row Sections
#'
#' As you add more rows and even other tables to the table, `index()` keeps
#' track of which rows belong to which section. This is useful for functions
#' like [kableExtra::pack_rows()] which requires a named numeric vector.
#'
#' If we had a crosstab for ages, by the time we are done the index might groups
#' the rows into sections like `c(Age = 2, ANOVA = 1)`, meaning the first 2 rows
#' will be under the heading Age and the next 1 is under the heading ANOVA.
#'
#' Setting `long = TRUE` will convert the index to a character vector with one
#' value for each row, like `c("Age", "Age", "ANOVA")`.
#'
#' The setter `index(ct) <- new_index` sets a custom index and can be passed
#' input in either condensed (named numeric vector) or long (character vector)
#' form. The condensed form must have the same sum as there are rows in ct, and
#' the long form must have the same length as there are rows in ct.
#'
#' @param ct The crosstab you want to create the index for
#' @param long Logical, setting `TRUE` converts the condensed numeric intex to a character index with as many values as `ct` has rows.
#' @param value The replacement index vector
#'
#' @returns A vector describing the sections or groups of rows, especially useful for kableExtra::pack_rows()
#' @export
#'
#' @examples
#' num_ct <- crosstab(length_by_species, cohort_col_name = "species") |>
#'     add_complete_total_row() |>
#'     add_mean_sd_row() |>
#'     add_med_q1_q3_row() |>
#'     add_anova_rows()
#'
#' num_ct # The output table
#' index(num_ct) # The condensed row groupings
#' index(num_ct, long = TRUE) # The long row groupings
#'
#' index(num_ct) <- c("sec1" = 2, "sec2" = 2, "sec3" = 2)
#' index(num_ct)
#'
index <- function(ct, long = FALSE) {
    validate_input_index_getter(ct, long)
    out <- attr(ct, "index")

    if (is.null(out) | long) {
        return(out)
    } else if (length(out) == 0) {
        return(stats::setNames(numeric(0), character(0)))
    } else {
        rle_result <- rle(out)
        return(stats::setNames(rle_result$lengths, rle_result$values))
    }
}

table_id <- function(ct, long = FALSE) {
    validate_input_table_id_getter(ct, long)
    out <- attr(ct, "table_id")

    if (is.null(out) | long) {
        return(out)
    } else if (length(out) == 0) {
        return(stats::setNames(numeric(0), character(0)))
    } else {
        rle_result <- rle(out)
        return(stats::setNames(rle_result$lengths, rle_result$values))
    }
}

table_name <- function(ct, long = FALSE) {
    validate_input_table_name_getter(ct, long)
    out <- attr(ct, "table_name")

    if (is.null(out) | long) {
        return(out)
    } else if (length(out) == 0) {
        return(stats::setNames(character(0), character(0)))
    } else {
        rle_result <- rle(out)
        return(stats::setNames(rle_result$lengths, rle_result$values))
    }
}

table_type <- function(ct, long = FALSE) {
    validate_input_table_type_getter(ct, long)
    out <- attr(ct, "table_type")

    if (is.null(out) | long) {
        return(out)
    } else if (length(out) == 0) {
        return(stats::setNames(character(0), character(0)))
    } else {
        rle_result <- rle(out)
        return(stats::setNames(rle_result$lengths, rle_result$values))
    }
}

footnotes <- function(ct) {
    attr(ct, "footnotes")
}

manual_escape <- function(ct) {
    attr(ct, "manual_escape")
}

#' @noRd
#' @export
desc_name.crosstab <- function(ct_data) {
    desc_name(data_table(ct_data))
}

#' @noRd
#' @export
var_name.crosstab <- function(ct_data) {
    var_name(data_table(ct_data))
}

#' @noRd
#' @export
var.crosstab <- function(ct_data, raw = FALSE) {
    var(data_table(ct_data), raw = raw)
}

#' @noRd
#' @export
var_levels.crosstab <- function(ct_data) {
    var_levels(data_table(ct_data))
}

#' @noRd
#' @export
cohort_name.crosstab <- function(ct_data) {
    cohort_name(data_table(ct_data))
}

#' @noRd
#' @export
cohort.crosstab <- function(ct_data, raw = FALSE) {
    cohort(data_table(ct_data), raw = raw)
}

#' @noRd
#' @export
cohort_levels.crosstab <- function(ct_data, raw = FALSE) {
    cohort_levels(data_table(ct_data), raw = raw)
}

#' @noRd
#' @export
var_map.crosstab <- function(ct_data) {
    var_map(data_table(ct_data))
}

#' @noRd
#' @export
var_mapped.crosstab <- function(ct_data, raw = FALSE) {
    var_mapped(data_table(ct_data), raw = raw)
}

#' @noRd
#' @export
combined_cohort_name.crosstab <- function(ct_data) {
    combined_cohort_name(data_table(ct_data))
}

#' @noRd
#' @export
get_raw_data.crosstab <- function(ct_data) {
    get_raw_data(data_table(ct_data))
}

# SETTERS ####
#' @rdname data_table
#' @export
`data_table<-` <- function(ct, value) {
    assert_crosstab(ct, strict = TRUE)
    UseMethod("data_table<-", value)
}

#' @noRd
#' @export
`data_table<-.crosstab_data` <- function(ct, value) {
    validate_input_data_table_setter(ct, value)
    attr(ct, "data") <- value
    return(ct)
}

#' @noRd
#' @export
`data_table<-.crosstab` <- function(ct, value) {
    data_table(ct) <- data_table(value)
    return(ct)
}

#' @rdname data_table
#' @export
set_new_data <- function(ct, tbl, var_map = NULL) {
    UseMethod("set_new_data", tbl)
}

#' @noRd
#' @export
set_new_data.crosstab <- function(ct, tbl, var_map = NULL) {
    ct <- set_new_data(ct, data_table(tbl), var_map = var_map)

    if (is.crosstab(tbl, strict = TRUE)) {
        footnotes1 <- attr(ct, "footnotes")
        footnotes2 <- attr(tbl, "footnotes")
        new_footnotes <- c(footnotes1, footnotes2)
        new_footnotes[["likert"]] <- c(footnotes1[["likert"]], footnotes2[["likert"]])

        attr(ct, "footnotes") <- new_footnotes
    }

    return(ct)
}

#' @noRd
#' @export
set_new_data.crosstab_data <- function(ct, tbl, var_map = NULL) {
    data_table(ct) <- tbl
    return(ct)
}

#' @noRd
#' @export
set_new_data.data.frame <- function(ct, tbl, var_map = NULL) {
    validate_input_set_new_data_df(ct, tbl, var_map)
    new_data <- crosstab_data(
        df = tbl,
        cohort_col_name = cohort_name(ct),
        var_map = var_map,
        combined_cohort_name = combined_cohort_name(ct),
        desc_col_name = desc_name(ct)
    )
    data_table(ct) <- new_data
    return(ct)
}

#' @rdname index
#' @export
`index<-` <- function(ct, value) {
    validate_input_index_setter(ct, value)
    if (is.numeric(value))
        value <- rep(names(value), times = value)
    attr(ct, "index") <- value
    return(ct)
}

`table_id<-` <- function(ct, value) {
    validate_input_table_id_setter(ct, value)
    attr(ct, "table_id") <- value
    return(ct)
}

`table_name<-` <- function(ct, value) {
    validate_input_table_name_setter(ct, value)
    attr(ct, "table_name") <- value
    return(ct)
}

`table_type<-` <- function(ct, value) {
    validate_input_table_type_setter(ct, value)
    attr(ct, "table_type") <- value
    return(ct)
}

`manual_escape<-` <- function(ct, value) {
    validate_input_manual_escape_setter(ct, value)
    attr(ct, "manual_escape") <- value
    return(ct)
}

#' @noRd
#' @export
`var_name<-.crosstab` <- function(ct_data, value) {
    `var_name<-`(data_table(ct_data), value)
}

#' @noRd
#' @export
`var<-.crosstab` <- function(ct_data, value) {
    `var<-`(data_table(ct_data), value)
}

#' @noRd
#' @export
`var_levels<-.crosstab` <- function(ct_data, value) {
    `var_levels<-`(data_table(ct_data), value)
}

#' @noRd
#' @export
`cohort_name<-.crosstab` <- function(ct_data, value) {
    `cohort_name<-`(data_table(ct_data), value)
}

#' @noRd
#' @export
`cohort<-.crosstab` <- function(ct_data, value) {
    `cohort<-`(data_table(ct_data), value)
}

#' @noRd
#' @export
`cohort_levels<-.crosstab` <- function(ct_data, value) {
    `cohort_levels<-`(data_table(ct_data), value)
}

#' @noRd
#' @export
`var_map<-.crosstab` <- function(ct_data, value) {
    `var_map<-`(data_table(ct_data), value)
}

# UTILITIES ####
#' Combine Multiple Crosstabs
#'
#' `stack_crosstabs()` takes crosstab objects and combines them into one
#' crosstab in the order that they are provided. The resulting crosstab's data
#' table will be the same as the final crosstab passed to this function.
#'
#' @param ... As many crosstab objects as you want to combine, separated by
#' commas
#'
#' @returns One crosstab with all rows from all crosstabs passed in, and the data table from the final crosstab
#' @export
#'
#' @examples
#' ct1 <- iris[, c("Sepal.Length", "Species"), drop = FALSE] |>
#'     crosstab(cohort_col_name = "Species") |>
#'     add_default_table()
#'
#' ct2 <- iris[, c("Petal.Length", "Species"), drop = FALSE] |>
#'     crosstab(cohort_col_name = "Species") |>
#'     add_default_table()
#'
#' ct3 <- iris[, c("Sepal.Width", "Species"), drop = FALSE] |>
#'     crosstab(cohort_col_name = "Species") |>
#'     add_default_table()
#'
#' ct4 <- iris[, c("Petal.Width", "Species"), drop = FALSE] |>
#'     crosstab(cohort_col_name = "Species") |>
#'     add_default_table()
#'
#' combined_ct <- stack_crosstabs(ct1, ct2, ct3, ct4)
#'
#' combined_ct
#'
stack_crosstabs <- function(...) {
    cts <- list(...)
    validate_input_stack_crosstabs(cts)

    output_table <- Reduce(function(ct1, ct2) {
        ct1 <- set_new_data(ct1, ct2)
        ct1 <- add_rows(ct1, ct2)

        footnotes1 <- attr(ct1, "footnotes")
        footnotes2 <- attr(ct2, "footnotes")
        new_footnotes <- c(footnotes1, footnotes2)
        new_footnotes[["likert"]] <- c(footnotes1[["likert"]], footnotes2[["likert"]])

        attr(ct1, "footnotes") <- new_footnotes
        return(ct1)
    }, cts)

    return(output_table)
}
