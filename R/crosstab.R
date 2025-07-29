# CONSTRUCTORS ####
new_crosstab <- function(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name, new_var_col_name = NULL) {
    validate_input_new_crosstab(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name)
    data <- crosstab_data(
        df = df,
        cohort_col_name = cohort_col_name,
        var_map = var_map,
        new_var_col_name = new_var_col_name,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name
    )

    structure(
        data.frame(), # Output table
        data = data,  # crosstab_data
        index = character(0),
        table_id = numeric(0),
        footnotes = list(),
        manual_escape = F,
        class = c(CT_CLASS, class(data.frame()))
    )
}

# HELPERS ####
#' @export
crosstab <- function(df, cohort_col_name = NULL, var_map = NULL, new_var_col_name = NULL, combined_cohort_name = "All", desc_col_name = "Description") {
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

# GETTERS ####
#' @export
data_table <- function(ct, raw = F) {
    validate_input_data_table_getter(ct, raw)
    ct_data <- attr(ct, "data")
    if (raw) ct_data <- get_raw_data(ct_data)
    return(ct_data)
}

#' @export
index <- function(ct, long = F) {
    validate_input_index_getter(ct, long)
    out <- attr(ct, "index")

    if (is.null(out) | long) {
        return(out)
    } else if (length(out) == 0) {
        return(setNames(numeric(0), character(0)))
    } else {
        rle_result <- rle(out)
        return(setNames(rle_result$lengths, rle_result$values))
    }
}

table_id <- function(ct, long = F) {
    validate_input_table_id_getter(ct, long)
    out <- attr(ct, "table_id")

    if (is.null(out) | long) {
        return(out)
    } else if (length(out) == 0) {
        return(setNames(numeric(0), character(0)))
    } else {
        rle_result <- rle(out)
        return(setNames(rle_result$lengths, rle_result$values))
    }

}

footnotes <- function(ct) {
    attr(ct, "footnotes")
}

manual_escape <- function(ct) {
    attr(ct, "manual_escape")
}

#' @export
desc_name.crosstab <- function(ct_data) {
    desc_name(data_table(ct_data))
}

#' @export
var_name.crosstab <- function(ct_data) {
    var_name(data_table(ct_data))
}

#' @export
var.crosstab <- function(ct_data, raw = F) {
    var(data_table(ct_data), raw = raw)
}

#' @export
var_levels.crosstab <- function(ct_data) {
    var_levels(data_table(ct_data))
}

#' @export
cohort_name.crosstab <- function(ct_data) {
    cohort_name(data_table(ct_data))
}

#' @export
cohort.crosstab <- function(ct_data, raw = F) {
    cohort(data_table(ct_data), raw = raw)
}

#' @export
cohort_levels.crosstab <- function(ct_data, raw = F) {
    cohort_levels(data_table(ct_data), raw = raw)
}

#' @export
var_map.crosstab <- function(ct_data) {
    var_map(data_table(ct_data))
}

#' @export
var_mapped.crosstab <- function(ct_data, raw = F) {
    var_mapped(data_table(ct_data), raw = raw)
}

#' @export
combined_cohort_name.crosstab <- function(ct_data) {
    combined_cohort_name(data_table(ct_data))
}

#' @export
get_raw_data.crosstab <- function(ct_data) {
    get_raw_data(data_table(ct_data))
}

# SETTERS ####
#' @export
`data_table<-` <- function(ct, value) {
    assert_crosstab(ct, strict = T)
    UseMethod("data_table<-", value)
}

#' @export
`data_table<-.crosstab_data` <- function(ct, value) {
    validate_input_data_table_setter(ct, value)
    attr(ct, "data") <- value
    return(ct)
}

#' @export
`data_table<-.crosstab` <- function(ct, value) {
    data_table(ct) <- data_table(value)
    return(ct)
}

#' @export
set_new_data <- function(ct, tbl, var_map = NULL) {
    UseMethod("set_new_data", tbl)
}

#' @export
set_new_data.crosstab <- function(ct, tbl, var_map = NULL) {
    set_new_data(ct, data_table(tbl), var_map = var_map)

    # FIXME: Update likert footnotes
}

#' @export
set_new_data.crosstab_data <- function(ct, tbl, var_map = NULL) {
    data_table(ct) <- tbl
    return(ct)
}

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

#' @export
`index<-` <- function(ct, value) {
    validate_input_index_setter(ct, value)
    attr(ct, "index") <- value
    return(ct)
}

`table_id<-` <- function(ct, value) {
    validate_input_table_id_setter(ct, value)
    attr(ct, "table_id") <- value
    return(ct)
}

`manual_escape<-` <- function(ct, value) {
    validate_input_manual_escape_setter(ct, value)
    attr(ct, "manual_escape") <- value
    return(ct)
}

#' @export
`var_name<-.crosstab` <- function(ct_data, value) {
    `var_name<-`(data_table(ct_data), value)
}

#' @export
`var<-.crosstab` <- function(ct_data, value) {
    `var<-`(data_table(ct_data), value)
}

#' @export
`var_levels<-.crosstab` <- function(ct_data, value) {
    `var_levels<-`(data_table(ct_data), value)
}

#' @export
`cohort_name<-.crosstab` <- function(ct_data, value) {
    `cohort_name<-`(data_table(ct_data), value)
}

#' @export
`cohort<-.crosstab` <- function(ct_data, value) {
    `cohort<-`(data_table(ct_data), value)
}

#' @export
`cohort_levels<-.crosstab` <- function(ct_data, value) {
    `cohort_levels<-`(data_table(ct_data), value)
}

#' @export
`var_map<-.crosstab` <- function(ct_data, value) {
    `var_map<-`(data_table(ct_data), value)
}

# UTILITIES ####
#' @export
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
