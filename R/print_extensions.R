#' @export
#' @noRd
#' @import crayon
print.crosstab <- function(x, ...) {
    assert_crosstab(x)

    title <- blue $ bold
    cat(title("<Crosstab object>\n"))

    num_tables <- max(length(table_id(x)), 1)
    plural <- ifelse(num_tables == 1, "", "s")
    subtitle <- sprintf("# Contains %s table%s", num_tables, plural)

    if (is.crosstab.grouped(x))
        subtitle <- paste0(subtitle, ", grouped_by ", cyan(cohort_name(x)))

    subtitle <- paste0(subtitle, "\n")
    cat(silver(subtitle))

    print_table_string <- function(table_name, table_type) {
        bold_red <- bold $ red
        table_type <- sprintf("[%s]", table_type)

        str <- sprintf(" Table: %s %s\n", bold_red(table_name), silver(table_type))

        num_char <- nchar("Table:  ") + nchar(table_name) + nchar(table_type) + 2
        border <- paste0(paste(rep("=", num_char), collapse = ""), "\n")

        cat(border)
        cat(str)
        cat(border)
    }

    # If there are zero rows in the table
    if (nrow(x) == 0) {
        print_table_string(var_name(x), get_crosstab_type(x))
        cat(silver("  <empty table>\n\n"))
        return(invisible(x))
    }

    lines <- capture.output(print.data.frame(x))
    lines <- sub("^", "  ", lines)  # two spaces

    col_names_ind <- 1
    col_names <- c()
    while (col_names_ind <= length(lines)) {
        col_names <- c(col_names, lines[col_names_ind])
        lines <- lines[setdiff(1:length(lines), col_names_ind)]
        col_names_ind <- col_names_ind + nrow(x)
    }

    # If there is more than one row in the table
    indices <- data.frame(
            table_id = table_id(x, long = TRUE),
            section_name = index(x, long = TRUE),
            table_name = table_name(x, long = TRUE),
            table_type = table_type(x, long = TRUE),
            row_number = 1:nrow(x)
        ) |>
        dplyr::group_by(table_id, table_name, section_name) |>
        dplyr::summarise(
            num_rows = dplyr::n(),
            start_index = min(row_number),
            table_type = utils::head(table_type, 1),
            .groups = "drop"
        ) |>
        dplyr::arrange(start_index)

    cur_table_id <- 0
    for (i in 1:nrow(indices)) {
        table_id <- indices[i, "table_id", drop = TRUE]
        table_name <- indices[i, "table_name", drop = TRUE]
        table_type <- indices[i, "table_type", drop = TRUE]
        section_name <- indices[i, "section_name", drop = TRUE]
        num_rows <- indices[i, "num_rows", drop = TRUE]
        start_index <- indices[i, "start_index", drop = TRUE]

        # Print the table name at the start of each new table
        if (cur_table_id != table_id) {
            cur_table_id = table_id
            print_table_string(table_name, table_type)
        }

        if (section_name != table_name) {
            cat(silver(sprintf("  %s:\n", section_name)))
        }

        row_start <- start_index
        row_end <- start_index + num_rows - 1
        col_names_ind <- 1

        while (row_start <= length(lines)) {
            cat(paste0(col_names[col_names_ind], "\n"), sep = "")
            cat(paste0(lines[row_start:row_end], "\n"), sep = "")
            cat("\n")
            col_names_ind <- col_names_ind + 1
            row_start <- row_start + nrow(x)
            row_end <- row_end + nrow(x)
        }
    }

    table_names <- table_name(x, long = TRUE)
    if (var_name(x) != table_names[length(table_names)]) {
        print_table_string(var_name(x), get_crosstab_type(x))
        cat(silver("  <empty table>\n\n"))
    }

    invisible(x)
}

#' @export
#' @noRd
#' @import crayon
print.crosstab_data <- function(x, n = NULL, raw = NULL, ...) {
    assert_crosstab_data(x)

    if (is.null(n)) n <- 10
    assert_that(
        is.numeric(n),
        msg = "n must be numeric"
    )

    if (is.null(raw)) raw <- FALSE
    assert_that(
        is.logical(raw),
        msg = "raw must be logical"
    )

    if (raw) x <- get_raw_data(x)

    cat(red(sprintf("<%s Crosstab Data Object>\n", get_crosstab_type(x))))
    cat(silver(sprintf("# Variable name: %s\n", blue(var_name(x)))))
    if (is.crosstab.grouped(x))
        cat(silver(sprintf("# Grouped by: %s\n", blue(cohort_name(x)))))
    if (is.crosstab.likert(x)) {
        cat(silver("# Variable mapping:\n"))
        mappings <- var_map(x)
        cat(blue(sprintf("`%s` = %s\n", names(mappings), mappings)), sep = "")
    }

    if (is.crosstab.multi(x)) {
        temp <- x
        var_col_name <- var_name(temp)
        cur_var <- var(temp)

        # Format each item with as.character() to print properly
        cur_var <- lapply(cur_var, as.character)

        temp[[var_col_name]] <- cur_var
        print.data.frame(head(temp, n))
    } else {
        print.data.frame(head(x, n))
    }


    if (n < nrow(x)) {
        rows_remaining <- nrow(x) - n

        if (rows_remaining == 1) {
            cat(silver("# \u2139 1 more row\n"))
        } else {
            cat(silver(sprintf("# \u2139 %s more rows\n", rows_remaining)))
        }

        cat(silver("# \u2139 Use `print(n = ...)` to see more rows\n"))
    }

    if (is.crosstab.grouped(x) & !raw) {
        cat(silver(sprintf(
            "# \u2139 Use `print(raw = TRUE)` to remove combined cohort \"%s\"\n",
            combined_cohort_name(x)
        )))
    }

    invisible(x)
}
