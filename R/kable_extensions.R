add_footnote <- function(ct, type, text) {
    assert_crosstab(ct, strict = TRUE)
    assert_that(is.character(text))
    assert_that(
        is.character(type),
        type %in% c("anova", "chisq", "rao-scott", "likert", "symbol", "number", "alphabet")
    )

    cur_footnotes <- footnotes(ct)
    if (type == "likert")
        cur_footnotes[[type]] <- c(cur_footnotes[[type]], text)
    else
        cur_footnotes[[type]] <- text

    attr(ct, "footnotes") <- cur_footnotes

    return(ct)
}

add_anova_marker_footnotes <- function(ct, type, cutoff) {
    assert_crosstab(ct, strict = TRUE)
    if (is.null(type)) type <- "symbol"
    assert_that(
        is.character(type),
        length(type) == 1,
        type %in% c("number", "alphabet", "symbol")
    )
    assert_that(is.numeric(cutoff), cutoff > 0)

    # Generate description
    description_parts <- "Numeric data has p-values calculated by ANOVA test"
    if (get_anova_p_value(ct) <= cutoff)
        description_parts <- c(description_parts, "with Tukey post-hoc")
    if (cutoff != 0.05)
        description_parts <- c(description_parts, sprintf("(cutoff for significance set at %s)", cutoff))

    description <- paste(description_parts, collapse = " ")
    description <- paste0(description, ".")

    # Add marker descriptions
    comp_levels <- cohort_levels(ct, raw = TRUE)
    comp_levels <- comp_levels[1:(length(comp_levels)-1)]
    marker_descs <- paste0("Significantly different than \"", comp_levels, "\"")

    ct |>
        add_footnote(type = "anova", text = description) |>
        add_footnote(type = type, text = marker_descs)
}

add_anova_row_footnotes <- function(ct, cutoff) {
    assert_crosstab(ct, strict = TRUE)
    assert_that(is.numeric(cutoff), cutoff > 0)

    # Generate description
    description_parts <- "Numeric data has p-values calculated by ANOVA test"
    if (get_anova_p_value(ct) <= cutoff)
        description_parts <- c(description_parts, "with Tukey post-hoc")
    if (cutoff != 0.05)
        description_parts <- c(description_parts, sprintf("(cutoff for significance set at %s)", cutoff))

    description <- paste(description_parts, collapse = " ")
    description <- paste0(description, ".")

    ct |>
        add_footnote(type = "anova", text = description)
}

add_chisq_row_footnotes <- function(ct, p.adj, method, cutoff) {

    p_val_adjustments <- c(
        bonferroni = "Bonferroni",
        holm = "Holm",
        hochberg = "Hochberg",
        hommel = "Hommel",
        BH = "Benjamini-Hochberg",
        fdr = "Benjamini-Hochberg",
        BY = "Benjamini-Yekutieli"
    )

    assert_crosstab(ct, strict = TRUE)
    assert_that(is.numeric(cutoff), cutoff > 0)
    assert_that(is.logical(p.adj))
    assert_that(
        is.character(method),
        method %in% names(p_val_adjustments),
        msg = paste(
            "p.adj must be one of the following:\n\"",
            paste(names(p_val_adjustments), collapse = "\", \""),
            "\".",
            collapse = ""
        )
    )

    description_parts <- c("Categorical data has p-values calculated by Pearson chi-squared test")
    if (p.adj) {
        adj_text <- sprintf("%s adjustment", p_val_adjustments[method])
        connector <- if (is.crosstab.multi(ct)) "and" else "with"
        description_parts <- c(description_parts, paste(connector, adj_text))
    }
    if (cutoff != 0.05) {
        description_parts <- c(description_parts, sprintf("(cutoff for significance set at %s)", cutoff))
    }

    description <- paste(description_parts, collapse = " ")
    description <- paste0(description, ".")

    ct |>
        add_footnote(type = "chisq", text = description)
}

add_rao_scott_row_footnotes <- function(ct, p.adj, method, cutoff) {

    p_val_adjustments <- c(
        bonferroni = "Bonferroni",
        holm = "Holm",
        hochberg = "Hochberg",
        hommel = "Hommel",
        BH = "Benjamini-Hochberg",
        fdr = "Benjamini-Hochberg",
        BY = "Benjamini-Yekutieli"
    )

    assert_crosstab(ct, strict = TRUE)
    assert_that(is.numeric(cutoff), cutoff > 0)
    assert_that(is.logical(p.adj))
    assert_that(
        is.character(method),
        method %in% names(p_val_adjustments),
        msg = paste(
            "p.adj must be one of the following:\n\"",
            paste(names(p_val_adjustments), collapse = "\", \""),
            "\".",
            collapse = ""
        )
    )

    description_parts <- c("Multi-response data has p-values calculated by chi-squared test with Rao-Scott correction")
    if (p.adj) {
        adj_text <- sprintf("and %s adjustment", p_val_adjustments[method])
        description_parts <- c(description_parts, adj_text)
    }
    if (cutoff != 0.05) {
        description_parts <- c(description_parts, sprintf("(cutoff for significance set at %s)", cutoff))
    }

    description <- paste(description_parts, collapse = " ")
    description <- paste0(description, ".")

    ct |>
        add_footnote(type = "rao-scott", text = description)
}

add_likert_map_footnotes <- function(ct, var_map) {

    named_vector_to_text <- function(x) {
        if (is.null(names(x))) stop("Input vector must be named.")
        parts <- sprintf('"%s" = %s', names(x), as.character(x))
        paste0(paste(parts, collapse = ", "), ".")
    }

    likert_map = paste0(
        var_name(ct),
        ": ",
        named_vector_to_text(var_map)
    )

    ct |>
        add_footnote(type = "likert", text = likert_map)
}

#' Apply Auto-Generated Footnotes to Kable Objects from Crosstabs
#'
#' `apply_footnotes()` takes in kable objects created by [kbl()] and uses
#' [kableExtra::footnote()] to apply auto-generated footnotes (e.g. Likert maps,
#' legends for ANOVA markers, or extra info about statistical tests). Unless
#' specified otherwise, this function is automatically called by
#' [htcrosstabs::kbl()].
#'
#' @param kable_input A kable object created by `kbl()`
#' @param ct The associated crosstab used to create the kable object
#'
#' @returns The kable object with the footnotes added
#' @export
#'
#' @examples
#' test_ct <- crosstab(length_by_species, "species") |>
#'     add_default_table(anova_format = c("row", "marker"))
#'
#' printable_output <- kbl(test_ct, crosstab_footnotes = FALSE)
#' printable_output # Without footnotes
#'
#' printable_output <- apply_footnotes(printable_output, test_ct)
#' printable_output # With footnotes
#'
apply_footnotes <- function(kable_input, ct) {
    assert_that(inherits(kable_input, c("kableExtra", "knitr_kable")))
    assert_crosstab(ct, strict = TRUE)

    marker_types <- c("symbol", "alphabet", "number")
    new_kable <- kable_input

    saved_footnotes <- footnotes(ct)

    if (!is.null(saved_footnotes[["symbol"]]) |
        !is.null(saved_footnotes[["number"]]) |
        !is.null(saved_footnotes[["alphabet"]])) {

        new_kable <- new_kable |>
            kableExtra::footnote(
                symbol_title = "Marker Map: ",
                number_title = "",
                alphabet_title = "",
                symbol = saved_footnotes[["symbol"]],
                number = saved_footnotes[["number"]],
                alphabet = saved_footnotes[["alphabet"]],
                footnote_order = c("symbol", "alphabet", "number"),
                title_format = "bold",
                threeparttable = TRUE
            )
    }

    general <- c(
        saved_footnotes[["anova"]],
        saved_footnotes[["chisq"]],
        saved_footnotes[["rao-scott"]]
    )

    if (!is.null(general)) {
        new_kable <- new_kable |>
            kableExtra::footnote(
                general_title = "Statistical Testing Methods: ",
                title_format = "bold",
                general = general,
                threeparttable = TRUE
            )
    }

    likert <- saved_footnotes[["likert"]]

    if (!is.null(likert)) {
        new_kable <- new_kable |>
            kableExtra::footnote(
                general_title = "Likert Map: ",
                title_format = "bold",
                general = likert,
                threeparttable = TRUE
            )
    }

    return(new_kable)
}

#' Apply Auto-Generated Row Packing to Kable Objects from Crosstabs
#'
#' `apply_pack_rows()` takes in kable objects created by [kbl()] and uses
#' [kableExtra::pack_rows()] to group the rows into sections with an
#' auto-generated header (pulled from `index()`). Unless specified otherwise,
#' this function is automatically called by [htcrosstabs::kbl()].
#'
#' @param kable_input A kable object created by `kbl()`
#' @param ct The associated crosstab used to create the kable object
#'
#' @returns The kable object with the row grouping added
#' @export
#'
#' @examples
#' test_ct <- crosstab(length_by_species, "species") |>
#'     add_default_table(anova_format = c("row", "marker"))
#'
#' printable_output <- kbl(test_ct, pack_rows = FALSE)
#' printable_output # Without row grouping
#'
#' index(test_ct) # The index used for grouping
#'
#' printable_output <- apply_pack_rows(printable_output, test_ct)
#' printable_output # With row grouping
#'
apply_pack_rows <- function(kable_input, ct) {
    assert_that(inherits(kable_input, c("kableExtra", "knitr_kable")))
    assert_crosstab(ct, strict = TRUE)

    kableExtra::pack_rows(kable_input, index = index(ct))
}

#' Apply Lines Beetween Stacked Tables in Kable Objects from Crosstabs
#'
#' `apply_table_sep()` takes in kable objects created by [kbl()] and uses
#' [kableExtra::row_spec()] to apply horizontal lines between stacked tables.
#' Unless specified otherwise, this function is automatically called by
#' [htcrosstabs::kbl()].
#'
#' @param kable_input A kable object created by `kbl()`
#' @param ct The associated crosstab used to create the kable object
#'
#' @returns The kable object with the table-separating lines added
#' @export
#'
#' @examples
#' test_ct <- crosstab_stacked(iris, "Species")
#'
#' printable_output <- kbl(test_ct, table_sep = FALSE)
#' printable_output # Without lines
#'
#' printable_output <- apply_table_sep(printable_output, test_ct)
#' printable_output # With lines (likely won't be visible in Viewer window)
#'
apply_table_sep <- function(kable_input, ct) {
    assert_that(inherits(kable_input, c("kableExtra", "knitr_kable")))
    assert_crosstab(ct, strict = TRUE)

    rows_per_table <- table_id(ct)
    if (length(rows_per_table) > 1) {
        row_breaks <- cumsum(rows_per_table)[-length(rows_per_table)]
        out_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

        if (!is.null(out_format)) {
            if (out_format == "latex") {
                kable_input <- kable_input |>
                    kableExtra::row_spec(
                        row_breaks,
                        hline_after = TRUE
                    )
            } else if (out_format == "html") {
                kable_input <- kable_input |>
                    kableExtra::row_spec(
                        row_breaks,
                        extra_css = "border-bottom: 1px solid #ddd;"
                    )
            } else {
                warning("Unknown output format; no table lines added.")
            }
        }
    }

    return(kable_input)
}

#' Create Kable Tables from Crosstab Objects
#'
#' `kbl()` is a wrapper for [kableExtra::kbl()] and can be used as a drop-in
#' replacement. If any object besides a crosstab is passed in, it will simply
#' pass the input arguments on to [kableExtra::kbl()]. If a crosstab is passed
#' in, it can automatically apply auto-generated footnotes, row-packing, and
#' table separators.
#'
#' `kbl()` is a wrapper for [kableExtra::kbl()] and can be used as a drop-in
#' replacement. For crosstab objects, `kbl()` calls [apply_footnotes()],
#' [apply_pack_rows()], and [apply_table_sep()] unless otherwise specified in
#' the arguments.
#'
#' @section Superscripting:
#' If the crosstab object has ANOVA markers with `superscript = TRUE` then
#' `escape` is set to `FALSE` before passing the arguments on to
#' [kableExtra::kbl()]. This is because the superscripted markers have <sup>
#' HTML tags. `kbl()` automatically escapes these, but we want the raw output
#' passed directly to LaTeX. The entire table then has all LaTeX special
#' characters manually escaped, which will cause issues in the Viewer window,
#' but not in the final output.
#'
#' I could probably figure out how to make it work with the Viewer window, but
#' I'll be honest, I don't wanna. :)
#'
#' @param x The crosstab object, or other object to be passed to [kableExtra::kbl()]
#' @param crosstab_footnotes Logical - Add the auto-generated footnotes?
#' @param pack_rows Logical - Add the auto-generated row grouping?
#' @param table_sep Logical - Add the auto-generated lines separating stacked tables?
#' @param ... All other arguments to be passed to [kableExtra::kbl()]
#'
#' @returns The kable object returned from [kableExtra::kbl()]
#' @export
#'
#' @examples
#' library(kableExtra, warn.conflicts = FALSE)
#'
#' test_ct <- crosstab_stacked(
#'     iris,
#'     "Species",
#'     anova_format = c("row", "marker")
#' )
#'
#' # Default formatting
#' htcrosstabs::kbl(test_ct)
#'
#' # Use with other kableExtra formatting just like kableExtra::kbl()
#' test_ct |>
#'     htcrosstabs::kbl(
#'         booktabs = TRUE,
#'         align = c("l", "r", "r", "r", "r"),
#'         longtable = TRUE
#'     ) |>
#'     kableExtra::kable_styling(
#'         latex_options = c("striped", "repeat_header", "hold_position"),
#'         position = "center"
#'     )
#'
kbl <- function(x, crosstab_footnotes = TRUE, pack_rows = TRUE, table_sep = TRUE, ...) {
    UseMethod("kbl", x)
}

#' @noRd
#' @export
kbl.default <- function(x, crosstab_footnotes = TRUE, pack_rows = TRUE, table_sep = TRUE, ...) {
    args <- list(...)
    args[["x"]] <- x
    do.call(kableExtra::kbl, args = args)
}

#' @noRd
#' @export
kbl.crosstab <- function(x, crosstab_footnotes = TRUE, pack_rows = TRUE, table_sep = TRUE, ...) {
    assert_crosstab(x, strict = TRUE)
    assert_that(
        is.logical(crosstab_footnotes),
        is.logical(pack_rows),
        is.logical(table_sep)
    )

    args <- list(...)

    if (manual_escape(x)) {
        args$escape = FALSE
        args[["x"]] <- escape_table(x)
    } else {
        args[["x"]] <- x
    }

    kable_obj <- do.call(kableExtra::kbl, args = args)

    if (crosstab_footnotes) kable_obj <- apply_footnotes(kable_obj, x)

    if (pack_rows) kable_obj <- apply_pack_rows(kable_obj, x)

    if (table_sep) kable_obj <- apply_table_sep(kable_obj, x)

    return(kable_obj)
}
