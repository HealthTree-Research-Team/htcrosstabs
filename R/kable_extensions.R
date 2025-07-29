add_footnote <- function(ct, type, text) {
    assert_crosstab(ct, strict = T)
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
    assert_crosstab(ct, strict = T)
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
    comp_levels <- cohort_levels(ct, raw = T)
    comp_levels <- comp_levels[1:(length(comp_levels)-1)]
    marker_descs <- paste0("Significantly different than \"", comp_levels, "\"")

    ct |>
        add_footnote(type = "anova", text = description) |>
        add_footnote(type = type, text = marker_descs)
}

add_anova_row_footnotes <- function(ct, cutoff) {
    assert_crosstab(ct, strict = T)
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

    assert_crosstab(ct, strict = T)
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

    assert_crosstab(ct, strict = T)
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

#' @export
apply_footnotes <- function(kable_input, ct) {
    assert_that(inherits(kable_input, c("kableExtra", "knitr_kable")))
    assert_crosstab(ct, strict = T)

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
                title_format = "bold"
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
                general = general
            )
    }

    likert <- saved_footnotes[["likert"]]

    if (!is.null(likert)) {
        new_kable <- new_kable |>
            kableExtra::footnote(
                general_title = "Likert Map: ",
                title_format = "bold",
                general = likert
            )
    }

    return(new_kable)
}

#' @export
kbl <- function(x, crosstab_footnotes = T, pack_rows = T, lines_between_tables = T, ...) {
    UseMethod("kbl", x)
}

#' @export
kbl.default <- function(x, crosstab_footnotes = T, pack_rows = T, lines_between_tables = T, ...) {
    args <- list(...)
    args[["x"]] <- x
    do.call(kableExtra::kbl, args = args)
}

#' @export
kbl.crosstab <- function(x, crosstab_footnotes = T, pack_rows = T, lines_between_tables = T, ...) {
    assert_crosstab(x, strict = T)
    assert_that(
        is.logical(crosstab_footnotes),
        is.logical(pack_rows),
        is.logical(lines_between_tables)
    )

    args <- list(...)

    if (manual_escape(x)) {
        args$escape = F
        args[["x"]] <- escape_table(x)
    } else {
        args[["x"]] <- x
    }

    kable_obj <- do.call(kableExtra::kbl, args = args)

    if (crosstab_footnotes) {
        kable_obj <- apply_footnotes(kable_obj, x)
    }

    if (pack_rows) {
        kable_obj <- kableExtra::pack_rows(kable_obj, index = index(x))
    }

    rows_per_table <- table_id(x)
    if (lines_between_tables & length(rows_per_table) > 1) {
        row_breaks <- cumsum(rows_per_table)[-length(rows_per_table)]
        out_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

        if (!is.null(out_format)) {
            if (out_format == "latex") {
                kable_obj <- kable_obj |>
                    kableExtra::row_spec(
                        row_breaks,
                        hline_after = TRUE
                    )
            } else if (out_format == "html") {
                kable_obj <- kable_obj |>
                    kableExtra::row_spec(
                        row_breaks,
                        extra_css = "border-bottom: 1px solid #ddd;"
                    )
            } else {
                warning("Unknown output format; no table lines added.")
            }
        }
    }

    return(kable_obj)
}
