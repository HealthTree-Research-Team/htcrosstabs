# FUNCTIONS
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

insert_at <- function(vec, pos, values) {
    stopifnot(pos >= 1, pos <= length(vec) + 1)
    if (pos == 1) {
        return(c(values, vec))
    } else if (pos == length(vec) + 1) {
        return(c(vec, values))
    } else {
        return(c(vec[1:pos-1], values, vec[(pos):length(vec)]))
    }
}

escape_table = function(table) {
    escape_str = function(str) {
        # Combine HTML tags and entities
        html_pattern <- "(&[a-zA-Z0-9#]+;|<[^>]+>)"

        sapply(str, function(single_str) {
            # Split on HTML to isolate plain text parts
            pieces <- strsplit(single_str, html_pattern, perl = TRUE)[[1]]

            # Extract the HTML bits
            matches <- gregexpr(html_pattern, single_str, perl = TRUE)[[1]]
            html_parts <- if (matches[1] != -1) regmatches(single_str, list(matches))[[1]] else character(0)

            # Escape the plain text parts
            escaped_pieces <- sapply(pieces, Hmisc::latexTranslate)

            # Interleave escaped text and HTML parts
            result <- character(length = length(escaped_pieces) + length(html_parts))
            result[seq(1, length(result), by = 2)] <- escaped_pieces
            if (length(html_parts)) {
                result[seq(2, length(result), by = 2)] <- html_parts
            }

            paste(result, collapse = "")
        }, USE.NAMES = FALSE)
    }

    table[] <- lapply(table, function(col) {
        if (is.character(col) || is.factor(col)) {
            escape_str(as.character(col))
        } else {
            col
        }
    })
    table
}
