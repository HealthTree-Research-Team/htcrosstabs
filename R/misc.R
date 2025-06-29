pass <- function(obj) obj

remove_na <- function(obj) {
    obj[!is.na(obj)]
}

is.factorlist <- function(obj) {
    is.list(obj) && all(sapply(obj, function(x) is.factor(x)))
}

factor <- function(obj, levels = NULL, end_levels = NULL, ...) {
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

levels <- function(obj) {
    if (is.factorlist(obj))
        return(base::levels(unlist(obj)))
    else
        return(base::levels(obj))
}

`levels<-` <- function(obj, value) {
    if (is.list(obj))
        return(lapply(obj, function(x) base::`levels<-`(x, value)))
    else
        return(base::`levels<-`(obj, value))
}
