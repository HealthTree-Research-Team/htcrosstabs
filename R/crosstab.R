# CONSTRUCTORS ####
new_crosstab <- function(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name) {
    validate_input_new_crosstab(df, cohort_col_name, var_map, combined_cohort_name, desc_col_name)
    data <- crosstab_data(
        df = df,
        cohort_col_name = cohort_col_name,
        var_map = var_map,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name
    )

    index <- c(0)
    names(index) <- c(var_name(data))

    structure(
        data.frame(), # Output table
        data = data,  # crosstab_data
        index = index,
        class = c(CT_CLASS, class(data.frame()))
    )
}

# HELPERS ####
#' @export
crosstab <- function(df, cohort_col_name = NULL, var_map = NULL, combined_cohort_name = "All", desc_col_name = "Description") {
    ct <- new_crosstab(
        df = df,
        cohort_col_name = cohort_col_name,
        var_map = var_map,
        combined_cohort_name = combined_cohort_name,
        desc_col_name = desc_col_name
    )

    validate_crosstab(ct)
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
    if (long) out <- rep(names(out), times = out)
    return(out)
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
    new_index_val <- 0
    names(new_index_val) <- var_name(value)
    attr(ct, "index") <- c(attr(ct, "index"), new_index_val)
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
        return(ct1)
    }, cts)

    return(output_table)
}
