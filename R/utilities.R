# FUNCTIONS ####
pass <- function(obj) obj

remove_na <- function(obj) {
    obj[!is.na(obj)]
}

#' @export
is.factorlist <- function(obj) {
    is.list(obj) && all(sapply(obj, function(x) is.factor(x)))
}

#' @export
factor <- function(obj, levels = NULL, end_levels = NULL, drop_levels = F, ...) {
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
    if (is.list(obj))
        result <- lapply(obj, function(x) do.call(base::factor, c(list(x), dot_args)))
    else
        result <- do.call(base::factor, c(list(obj), dot_args))

    return(result)
}

#' @export
levels <- function(obj) {
    if (is.factorlist(obj))
        return(base::levels(unlist(obj)))
    else
        return(base::levels(obj))
}

#' @export
`levels<-` <- function(obj, value) {
    if (is.list(obj))
        return(lapply(obj, function(x) base::`levels<-`(x, value)))
    else
        return(base::`levels<-`(obj, value))
}

#' @export
default_var_map <- function(fct) {
    assert_that(is.factor(fct) | is.factorlist(fct), msg = "fct must be either a factor or list of factors")
    assert_that(!is.null(levels(fct)), msg = "fct has no \"levels\" attribute")
    lev <- levels(fct)
    assert_that(length(lev) > 0, msg = "levels attribute has length 0")
    vals <- (1:length(lev)) |> rev()
    names(vals) <- lev
    return(vals)
}

get_non_matching <- function(str, dif_than) {
    assert_that(is.character(str), is.character(dif_than))
    while (str %in% dif_than) {
        warning(sprintf(
            "Detected clash in name %s, adding %s",
            str,
            STR_CLASH_SUFFIX
        ))
        str <- paste0(str, STR_CLASH_SUFFIX)
    }
    return(str)
}

determine_col_type <- function(col, var_map = NULL) {
    if (!is.null(var_map))
        assert_that(is.numeric(var_map), !is.null(names(var_map)), msg = "var_map must be a named numeric vector")
    assert_that(!is.null(col), msg = "col must not be NULL")

    if (is.numeric(col))
        CT_DATA_CLASS_NUM
    else if (is.list(col))
        CT_DATA_CLASS_MULTI
    else if (all(col %in% c(names(var_map), NA)))
        CT_DATA_CLASS_LIKERT
    else
        CT_DATA_CLASS_CAT
}
