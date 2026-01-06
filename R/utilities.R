# FUNCTIONS
pass <- function(obj) obj

remove_na <- function(obj) {
    obj[!is.na(obj)]
}

#' Create Default Variable Map from Factor Levels
#'
#' Takes a factor or factorlist and assigns them Likert scores from
#' `length(fct)` to 1 in descending order, so a vector with the levels
#' `c("A", "B", "C")` would return `c(A = 3, B = 2, C = 1)`.
#'
#' @param fct A factor or factorlist
#'
#' @returns A named numeric vector with the factors mapped as names and their scores as values
#' @export
#'
#' @examples
#' levels(students$prof_support)
#' default_var_map(students$prof_support)
#'
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

#' Nest Column to List of Atomic Vectors
#'
#' [tidyr::nest()] condenses rows into lists of tibbles, but we need a list of
#' atomic vectors. This function groups rows by specified columns (or all other
#' columns if none specified) and collapses duplicate rows in the target column
#' into list entries.
#'
#' @param df A data frame to nest
#' @param multi_col_name The name of the column to nest (must be a single column name)
#' @param group_cols Optional character vector of column names to group by. If NULL
#'   (default), groups by all columns except `multi_col_name`. Multiple columns can
#'   be specified to create groupings based on combinations of those columns.
#'
#' @returns A data frame with the same columns in the same order as the input, but
#'   with `multi_col_name` converted to a list-column where each element contains
#'   the collapsed values from rows sharing the same grouping column values. The
#'   number of rows will equal the number of unique combinations of all columns
#'   except `multi_col_name` (or the specified `group_cols` if provided).
#'
#' @export
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom rlang :=
#'
#' @examples
#' # Create unnested data
#' unnested_df <- allergies_by_school
#' unnested_df[["id"]] <- 1:nrow(unnested_df)
#' unnested_df <- tidyr::unnest(unnested_df, allergies)
#' head(unnested_df, 5)
#'
#' # Nest data using default grouping (all columns except allergies)
#' nested_df <- nest_multi_col(unnested_df, "allergies")
#' head(nested_df, 5)
#'
#' # Nest data using specific grouping columns
#' nested_df <- nest_multi_col(unnested_df, "allergies", group_cols = c("id", "school"))
#' head(nested_df, 5)
#'
#' # Single column dataframe - wraps each value in a list
#' single_col <- data.frame(allergies = c("nuts", "eggs", "milk"))
#' nested_single <- nest_multi_col(single_col, "allergies")
#' nested_single
#'
nest_multi_col <- function(df, multi_col_name, group_cols = NULL) {

    # Input checks
    assert_that(is.data.frame(df))

    assert_that(is.character(multi_col_name))
    assert_that(length(multi_col_name) == 1, msg = "multi_col_name must have 1 value")
    assert_that(multi_col_name %in% names(df), msg = sprintf("`%s` not found in df", multi_col_name))

    assert_that(!is.list(df[[multi_col_name]]), msg = sprintf(
        "Column `%s` is already of type list; nest_multi_col expects an atomic vector column to nest into a list",
        multi_col_name
    ))

    if (!is.null(group_cols)) {
        assert_that(is.character(group_cols))
        assert_that(length(group_cols) > 0, msg = "group_cols cannot be a 0-length vector")
        assert_that(all(group_cols %in% names(df)), msg = sprintf(
            "The following group_cols were not found in df: %s",
            paste0("`", setdiff(group_cols, names(df)), "`", collapse = "`, `")
        ))
    }

    # Store original column order
    original_col_order <- names(df)

    # If no grouping columns specified, use all other columns
    if (is.null(group_cols) && (ncol(df) > 1)) {
        group_cols <- names(df)[names(df) != multi_col_name]
    }

    # Separate out the multi_col_name column and the grouping columns
    grouped_df <- df[, c(multi_col_name, group_cols), drop = FALSE]

    if (ncol(grouped_df) == 1) {

        # If there are no grouping columns, just wrap each value in a list
        df[[multi_col_name]] <- as.list(df[[multi_col_name]])

    } else {

        # Group by the grouping columns
        grouped_df <- dplyr::group_by(grouped_df, !!!rlang::syms(group_cols))

        grouped_df <- grouped_df |>
            dplyr::summarise(
                !!rlang::sym(multi_col_name) := list(!!rlang::sym(multi_col_name)),
                .groups = "drop"
            )

        other_col_names <- setdiff(names(df), c(multi_col_name, group_cols))

        # Join back any other columns that were not part of the grouping, so any
        # rows which happen to have differing values in those columns will be duplicated
        if (length(other_col_names) > 0) {
            other_cols_df <- df[, c(other_col_names, group_cols), drop = FALSE] |>
                dplyr::distinct()

            df <- dplyr::left_join(other_cols_df, grouped_df, by = group_cols)
        } else {
            df <- grouped_df
        }
    }

    # Restore original column order
    df <- df[, original_col_order, drop = FALSE]

    return(df)
}

duplicated_vals <- function(obj) {
    repeated_vals <- obj[duplicated(obj)]
    obj %in% repeated_vals
}

get_crosstab_type <- function(ct) {
    assert_crosstab(ct, strict = F)
    if (is.crosstab.categorical(ct)) "Categorical"
    else if (is.crosstab.numeric(ct)) "Numeric"
    else if (is.crosstab.likert(ct)) "Likert"
    else if (is.crosstab.multi(ct)) "Multi-Response"
    else "Unknown Type"
}
