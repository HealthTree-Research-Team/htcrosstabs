# IMPORTS ####
#' @import assertthat

# UTILITIES ####

add_rows <- function(ct, rows) {
    # Sanity checks
    assert_that(inherits(ct, CT_CLASS), is.data.frame(rows))

    # Combine rows
    combined <- rbind(ct, rows)

    # Restore attributes (except dimensions, row.names, and column names)
    orig_attrs <- attributes(ct)
    combined_attrs <- attributes(combined)
    to_restore <- setdiff(names(orig_attrs), c("row.names", "names", "class"))

    for (attr_name in to_restore)
        attr(combined, attr_name) <- orig_attrs[[attr_name]]

    # Restore class
    class(combined) <- orig_attrs$class

    return(combined)
}

to_wide <- function(df, source_ct) {
    validate_to_wide(df, source_ct)


}

validate_to_wide <- function(df, source_ct) {
    assert_that(is.data.frame(df))
    assert_that(!is.null(source_ct), msg = "source_ct can not be NULL")
    assert_that(
        inherits(source_ct, c(CT_CLASS, CT_DATA_CLASS)),
        msg = "source_ct must be of type crosstab or crosstab_data"
    )

    if (inherits(source_ct, CT_CLASS))
        validate_crosstab(source_ct)
    else
        validate_crosstab_data(source_ct)

    assert_that(
        cohort_name(source_ct) %in% names(df),
        msg = "cohort column name from source_ct not found in df"
    )

}

# ROWS ####
add_total_row <- function(ct) {
    assert_that(inherits(ct, CT_CLASS))

    # Join complete and total tables
    joined <- dplyr::full_join(get_complete(ct), get_total(ct), by = cohort_name(data(ct)))

    # Create the string column
    str <- joined |>
        dplyr::mutate(str = paste0(complete, " / ", total)) |>
        dplyr::select(cohort, str)

    # Pivot so the cohort names become the column names
    str_wide <- str |>
        tidyr::pivot_wider(names_from = cohort, values_from = str)

    # Add "Complete / Total" to the response column
    response_col <- response_col_name(ct)
    str_wide[[response_col]] <- "Complete / Total"

    # Put the response column at the beginning
    all_other_cols <- setdiff(names(str_wide), response_col)
    col_order <- c(response_col, all_other_cols)
    total_row <- str_wide[, col_order, drop = FALSE]

    # Convert it to a data.frame to avoid weirdness when combining with the original table
    total_row <- data.frame(total_row, check.names = F)

    # Add it to the running output
    ct <- add_rows(ct, total_row)

    return(ct)
}

