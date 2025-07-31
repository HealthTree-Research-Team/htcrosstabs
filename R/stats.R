# STATISTICAL TESTS ####
#' Perform Statistical Tests on Crosstab Objects
#'
#' `get_*_p_value()` functions take crosstab objects and return a single p-value
#' based on the given test: ANOVA for numeric and Likert-like data, chi-squared
#' categorical and Likert-like, and Rao-Scott adjusted chi-square for
#' multiresponse data. Post-hoc tests are performed by the [get_*_posthoc()]
#' functions.
#'
#' @section ANOVA Tests:
#' ANOVA tests are designed for numeric data and Likert-like data that has been
#' mapped to numeric. It can be performed on the other data types, but they are
#' cast first (also there would be very little info gained). The ANOVA test is
#' performed using [stats::aov()].
#'
#' @section Chi-Square Tests:
#' Chi-square tests are designed for categorical data and Likert-like data
#' (unmapped). Multi-response data is passed to the Rao-Scott adjusted
#' chi-square test with `get_rao_scott_p_value()`. It can be performed on
#' numeric data, but it is cast first (and there would be little information to
#' be gained). The chi-square test is performed using [stats::chisq.test()].
#'
#' @section Rao-Scott Adjusted Chi-Square Tests:
#' Rao-Scott adjusted chi-square tests are a corrected version of chi-square
#' tests used for multi-response data. It can be used on other types of
#' responses, but they are cast first, and the response will be essentially the
#' same as the normal chi-square test. The Rao-Scott adjusted test is
#' performed using [survey::svychisq()].
#'
#' @param data The crosstab or crosstab_data object
#'
#' @returns The specified p-value
#' @name get_p_values
#'
#' @examples
#' # ANOVA test for numeric data
#' num_ct <- crosstab(length_by_species, "species")
#'
#' get_anova_p_value(num_ct)
#'
#' # Chi-square test for categorical data
#' cat_ct <- crosstab(sports_by_age, "age")
#'
#' get_chisq_p_value(cat_ct)
#'
NULL

# ANOVA P-VALUE ####
#' @describeIn get_p_values Perform ANOVA test
#' @export
get_anova_p_value <- function(data) {
    UseMethod("get_anova_p_value", data)
}

#' @noRd
#' @export
get_anova_p_value.crosstab <- function(data) {
    get_anova_p_value(data_table(data))
}

#' @noRd
#' @export
get_anova_p_value.crosstab_data <- function(data) {
    get_anova_p_value(get_anova(data))
}

#' @noRd
#' @export
get_anova_p_value.aov <- function(data) {
    summary(data)[[1]][["Pr(>F)"]][1]
}

# CHI-SQUARE P-VALUE ####
#' @describeIn get_p_values Perform chi-square test
#' @export
get_chisq_p_value <- function(data) {
    UseMethod("get_chisq_p_value", data)
}

#' @noRd
#' @export
get_chisq_p_value.crosstab <- function(data) {
    get_chisq_p_value(data_table(data))
}

#' @noRd
#' @export
get_chisq_p_value.crosstab_data <- function(data) {
    warning("Casting data to categorical before performing chi-square test")
    get_chisq_p_value(suppressWarnings(as.crosstab.cat(data)))
}

#' @noRd
#' @export
get_chisq_p_value.crosstab_data_multi <- function(data) {
    get_rao_scott_p_value(data)
}

#' @noRd
#' @export
get_chisq_p_value.crosstab_data_likert <- function(data) {
    get_chisq_p_value(suppressWarnings(as.crosstab.cat(data)))
}

#' @noRd
#' @export
get_chisq_p_value.crosstab_data_cat <- function(data) {
    if (any(sapply(data, function(col) all(col == 0, na.rm = TRUE)))) {
        warning("Can not perform chi-square test when one cohort has 0 data points")
        return(NULL)
    }

    cohort_cols <- cohort_levels(data, raw = TRUE)

    counts <- get_count(data) |>
        to_wide(var_name(data), cohort_name(data), na_fill = 0)

    counts <- counts[, cohort_cols, drop = FALSE]

    return(get_chisq(counts))
}

# RAO-SCOTT P-VALUE ####
#' @describeIn get_p_values Perform Rao-Scott corrected chi-square test
#' @export
get_rao_scott_p_value <- function(data) {
    UseMethod("get_rao_scott_p_value", data)
}

#' @noRd
#' @export
get_rao_scott_p_value.crosstab <- function(data) {
    get_rao_scott_p_value(data_table(data))
}

#' @noRd
#' @export
get_rao_scott_p_value.crosstab_data <- function(data) {
    get_rao_scott_p_value(suppressWarnings(as.crosstab.multi(data)))
}

#' @noRd
#' @export
get_rao_scott_p_value.crosstab_data_multi <- function(data) {
    raw_data <- get_raw_data(data)
    get_rao_scott(raw_data, var_name(raw_data), cohort_name(raw_data))
}

# POST-HOC TESTS ####
#' Perform Post-Hoc Tests on Crosstab Objects
#'
#' @description
#' `get_*_posthoc()` functions take crosstab objects and return a symmetric
#' data frame/matrix of pairwise comparison p-values based on the given test:
#' ANOVA for numeric and Likert-like data, chi-squared categorical and
#' Likert-like, and Rao-Scott adjusted chi-square for multiresponse data.
#' Overall statistical tests to get a single p-value for all data collectively
#' are performed by the [get_*_p_value()] functions.
#'
#' The p-values from the multiple comparisons can be corrected using methods
#' such as Bonferroni, Benjamini-Hochberg, etc. using the `method` parameter.
#' P-values are adjusted using Benjamini Hochberg by default, but if no
#' adjustment is desired, set `p.adj = FALSE`.
#'
#' @inheritSection get_p_values ANOVA Tests
#'
#' @inheritSection get_p_values Chi-Square Tests
#'
#' @inheritSection get_p_values Rao-Scott Adjusted Chi-Square Tests
#'
#' @param data The crosstab or crosstab_data object
#' @param p.adj Logical - Should the p-values be adjusted to correct for multiple tests?
#' @param method Character - The p-value adjustment method (see the man page for [stats::p.adjust()] for possible values)
#'
#' @returns A symmetric data frame/matrix with pairwise post-hoc p-values
#' @name get_posthoc
#'
#' @examples
#' # ANOVA test for numeric data
#' num_ct <- crosstab(length_by_species, "species")
#'
#' get_tukey_posthoc(num_ct)
#'
#' # Chi-square test for categorical data
#' cat_ct <- crosstab(sports_by_age, "age")
#'
#' get_chisq_posthoc(cat_ct)
#'
NULL

# ANOVA POSTHOC ####
#' @describeIn get_posthoc Perform Tukey post-hoc
#' @export
get_tukey_posthoc <- function(data) {
    UseMethod("get_tukey_posthoc", data)
}

#' @noRd
#' @export
get_tukey_posthoc.crosstab <- function(data) {
    get_tukey_posthoc(data_table(data))
}

#' @noRd
#' @export
get_tukey_posthoc.crosstab_data <- function(data) {
    get_tukey_posthoc(get_anova(data))
}

#' @noRd
#' @export
get_tukey_posthoc.aov <- function(data) {
    tukey_results <- stats::TukeyHSD(data)
    p_vals <- tukey_results[[1]][, "p adj", drop = TRUE]
    comparison_names <- names(p_vals)
    comparisons <- strsplit(comparison_names, "-", fixed = TRUE)
    assert_that(
        all(sapply(comparisons, length) == 2),
        msg = "Cannot perform a Tukey posthoc if group-levels have a dash in them"
    )

    all_cohorts <- c(unique(sapply(comparisons, function(x) x[[2]])))
    all_cohorts <- c(all_cohorts, setdiff(sapply(comparisons, function(x) x[[1]]), all_cohorts))

    cohort_matrix <- matrix(
        1,
        nrow = length(all_cohorts),
        ncol = length(all_cohorts),
        dimnames = list(all_cohorts, all_cohorts)
    )

    for (i in seq_along(p_vals)) {
        pair <- comparisons[[i]]
        cohort_matrix[pair[1], pair[2]] <- p_vals[i]
        cohort_matrix[pair[2], pair[1]] <- p_vals[i]
    }

    cohort_df <- data.frame(cohort_matrix, check.names = FALSE)

    structure(cohort_df, class = c("posthoc", class(cohort_df)))
}

#' @describeIn get_posthoc Perform Tukey post-hoc (same as `get_tukey_posthoc()`)
#' @export
get_anova_posthoc <- function(data) {
    get_tukey_posthoc(data)
}

# CHI-SQUARE POSTHOC ####
#' @describeIn get_posthoc Perform chi-square post-hoc
#' @export
get_chisq_posthoc <- function(data, p.adj = TRUE, method = "BH") {
    UseMethod("get_chisq_posthoc", data)
}

#' @noRd
#' @export
get_chisq_posthoc.crosstab <- function(data, p.adj = TRUE, method = "BH") {
    get_chisq_posthoc(data_table(data), p.adj = p.adj, method = method)
}

#' @noRd
#' @export
get_chisq_posthoc.crosstab_data <- function(data, p.adj = TRUE, method = "BH") {
    warning("Casting data to categorical before performing chi-square test")
    get_chisq_posthoc(suppressWarnings(as.crosstab.cat(data)), p.adj = p.adj, method = method)
}

#' @noRd
#' @export
get_chisq_posthoc.crosstab_data_multi <- function(data, p.adj = TRUE, method = "BH") {
    get_rao_scott_posthoc(data, p.adj = p.adj, method = method)
}

#' @noRd
#' @export
get_chisq_posthoc.crosstab_data_likert <- function(data, p.adj = TRUE, method = "BH") {
    get_chisq_posthoc(suppressWarnings(as.crosstab.cat(data)), p.adj = p.adj, method = method)
}

#' @noRd
#' @export
get_chisq_posthoc.crosstab_data_cat <- function(data, p.adj = TRUE, method = "BH") {
    assert_that(is.logical(p.adj))
    assert_that(is.character(method))

    cohort_cols <- cohort_levels(data, raw = TRUE)

    counts <- get_count(data) |>
        to_wide(var_name(data), cohort_name(data), na_fill = 0)

    cohort_matrix <- matrix(
        data = 1,
        nrow = length(cohort_cols),
        ncol = length(cohort_cols),
        dimnames = list(cohort_cols, cohort_cols)
    )

    combos <- utils::combn(cohort_cols, 2, simplify = FALSE)
    p_vals <- c()
    for (combo in combos) {
        p_vals <- c(p_vals, get_chisq(counts[, combo, drop = FALSE]))
    }

    if (p.adj) p_vals <- stats::p.adjust(p_vals, method = method)

    for (i in 1:length(p_vals)) {
        p_val <- p_vals[i]
        combo <- combos[i][[1]]
        cohort_matrix[combo[1], combo[2]] <- p_val
        cohort_matrix[combo[2], combo[1]] <- p_val
    }

    empty_cohorts <- names(counts)[sapply(counts, function(col) all(col == 0, na.rm = TRUE))]

    for (empty_cohort in empty_cohorts)
        cohort_matrix[empty_cohort, empty_cohort] <- NA

    cohort_matrix <- data.frame(cohort_matrix, check.names = FALSE)

    structure(cohort_matrix, class = c("posthoc", class(cohort_matrix)))
}

# RAO-SCOTT POSTHOC ####
#' @describeIn get_posthoc Perform Rao-Scott corrected chi-square post-hoc
#' @export
get_rao_scott_posthoc <- function(data, p.adj = TRUE, method = "BH") {
    UseMethod("get_rao_scott_posthoc", data)
}

#' @noRd
#' @export
get_rao_scott_posthoc.crosstab <- function(data, p.adj = TRUE, method = "BH") {
    get_rao_scott_posthoc(data_table(data), p.adj = p.adj, method = method)
}

#' @noRd
#' @export
get_rao_scott_posthoc.crosstab_data <- function(data, p.adj = TRUE, method = "BH") {
    get_rao_scott_posthoc(suppressWarnings(as.crosstab.multi(data)), p.adj = p.adj, method = method)
}

#' @noRd
#' @export
get_rao_scott_posthoc.crosstab_data_multi <- function(data, p.adj = TRUE, method = "BH") {
    assert_that(is.logical(p.adj))
    assert_that(is.character(method))

    cohort_cols <- cohort_levels(data, raw = TRUE)

    cohort_matrix <- matrix(
        data = 1,
        nrow = length(cohort_cols),
        ncol = length(cohort_cols),
        dimnames = list(cohort_cols, cohort_cols)
    )

    combos <- utils::combn(cohort_cols, 2, simplify = FALSE)
    p_vals <- c()
    for (combo in combos) {
        keep <- cohort(data) == combo[1] | cohort(data) == combo[2]
        filtered_data <- data[keep, , drop = FALSE]
        filtered_data[[cohort_name(data)]] <- as.character(filtered_data[[cohort_name(data)]])

        if (length(unique(filtered_data[[cohort_name(data)]])) < 2)
            p_vals <- c(p_vals, NA)
        else
            p_vals <- c(p_vals, get_rao_scott(filtered_data, var_name(data), cohort_name(data)))
    }

    if (p.adj) p_vals <- stats::p.adjust(p_vals, method = method)

    for (i in 1:length(p_vals)) {
        p_val <- p_vals[i]
        combo <- combos[i][[1]]
        cohort_matrix[combo[1], combo[2]] <- p_val
        cohort_matrix[combo[2], combo[1]] <- p_val
    }

    empty_cohorts <- sapply(cohort_cols, function(x) sum(cohort(data) == x) == 0)
    empty_cohorts <- cohort_cols[empty_cohorts]

    for (empty_cohort in empty_cohorts)
        cohort_matrix[empty_cohort, empty_cohort] <- NA

    cohort_matrix <- data.frame(cohort_matrix, check.names = FALSE)

    structure(cohort_matrix, class = c("posthoc", class(cohort_matrix)))
}

# UTILITIES ####
p_value_categories <- function(p_value, cutoff = 0.05, round_to = 3) {
    if (is.na(p_value)) "NA"
    else if (p_value < 0.001 & cutoff >= 0.001) "< 0.001"
    else if (p_value <= cutoff) round(p_value, digits = round_to)
    else sprintf("NS (%s)", round(p_value, digits = round_to))
}

remove_zero_rows <- function(df) {
    keep <- !apply(df, 1, function(row) all(row == 0))
    df <- df[keep, , drop = FALSE]
    df
}

create_stat_row_skeleton <- function(data) {
    cohorts <- cohort_levels(data, raw = FALSE)
    cohorts_raw <- cohort_levels(data, raw = TRUE)

    row_names <- cohorts_raw[1:(length(cohorts_raw)-1)]
    desc_col <- paste0("Dif. ", row_names)
    row_names <- c(row_names, "Overall")
    desc_col <- c(desc_col, "Overall")

    col_names <- c(desc_name(data), cohorts)
    new_rows <- matrix(
        data = "-",
        nrow = length(row_names),
        ncol = length(col_names),
        dimnames = list(row_names, col_names)
    )
    new_rows[, desc_name(data)] <- desc_col
    return(new_rows)
}

fill_stat_row_skeleton <- function(new_rows, data, posthoc, overall_p_value, cutoff = 0.05, round_to = 3) {

    new_rows["Overall", 2] <- p_value_categories(overall_p_value, cutoff = cutoff, round_to = round_to)
    if (overall_p_value > cutoff) {
        new_rows <- new_rows["Overall", , drop = FALSE]
        rownames(new_rows) <- NULL
        return(data.frame(new_rows, check.names = FALSE))
    }

    for (comb in utils::combn(cohort_levels(data, raw = TRUE), 2, simplify = FALSE)) {
        row <- comb[1]
        col <- comb[2]
        new_rows[row, col] <- p_value_categories(posthoc[row, col], cutoff = cutoff, round_to = round_to)
    }

    new_rows <- data.frame(new_rows, check.names = FALSE)
    rownames(new_rows) <- NULL

    return(data.frame(new_rows, check.names = FALSE))
}

get_markers <- function(num_markers, marker_type = NULL, superscript = TRUE) {
    if (is.null(marker_type)) marker_type <- "symbol"
    assert_that(
        is.character(marker_type),
        length(marker_type) == 1,
        marker_type %in% c("symbol", "alphabet", "number"),
        msg = "marker_type must be either \"symbol\", \"alphabet\", or \"number\""
    )
    assert_that(is.logical(superscript))

    if (marker_type == "alphabet")
        return(get_alphabet(num_markers, superscript))
    else if (marker_type == "number")
        return(get_numbers(num_markers, superscript))
    else
        return(get_symbols(num_markers, superscript))
}

get_alphabet <- function(num_markers, superscript = TRUE) {
    assert_that(is.numeric(num_markers), num_markers >= 0, msg = "num_markers must be a non-zero number")
    if (num_markers == 0) return(NULL)
    alphabet <- extend_characters(letters, num_markers)
    if (superscript) alphabet <- sprintf("<sup>%s</sup>", alphabet)
    return(alphabet)
}

get_numbers <- function(num_markers, superscript = TRUE, as.character = TRUE) {
    assert_that(is.numeric(num_markers), num_markers >= 0, msg = "num_markers must be a non-zero number")
    if (num_markers == 0) return(NULL)
    numbers <- 1:num_markers
    if (superscript) numbers <- sprintf("<sup>%s</sup>", numbers)
    if (as.character) numbers <- as.character(numbers)
    return(numbers)
}

get_symbols <- function(num_markers, superscript = TRUE) {
    assert_that(is.numeric(num_markers), num_markers >= 0, msg = "num_markers must be a non-zero number")
    if (num_markers == 0) return(NULL)
    if (superscript)
        symbols <- c("*", "&dagger;", "&Dagger;", "&sect;", "&para;")
    else
        symbols <- c("*", "\u2020", "\u2021", "\u00a7", "\u00b6")
    symbols <- extend_characters(symbols, num_markers)
    if (superscript) symbols <- sprintf("<sup>%s</sup>", symbols)
    return(symbols)
}

extend_characters <- function(c, n) {
    assert_that(is.numeric(n), n >= 0, msg = sprintf("Can not create vector with %s characters", n))
    if (n == 0) return(NULL)

    output <- NULL
    repeats <- 0
    while (length(output) < n) {
        repeats <- repeats + 1
        output <- c(output, multiply_characters(c, repeats))
    }
    return(output[1:n])
}

multiply_characters <- function(c, n) {
    assert_that(n >= 0, msg = sprintf("Can not repeat characters %s times", n))
    sapply(c, function(x) {
        paste(rep(x, n), collapse = "")
    }, USE.NAMES = FALSE)
}

chisq_assumptions_met <- function(df) {
    if (nrow(df) == 0 | ncol(df) == 0) return(FALSE)
    expected_values <- outer(rowSums(df), colSums(df)) / sum(df)
    return(all(expected_values >= 5))
}

# ANOVA
get_anova <- function(ct_data) {
    UseMethod("get_anova", ct_data)
}

#' @noRd
#' @export
get_anova.crosstab <- function(ct_data) {
    get_anova(data_table(ct_data))
}

#' @noRd
#' @export
get_anova.crosstab_data_likert <- function(ct_data) {
    get_anova(suppressWarnings(as.crosstab.num(ct_data)))
}

#' @noRd
#' @export
get_anova.crosstab_data <- function(ct_data) {
    warning("Casting data to numeric before performing ANOVA test")
    get_anova(as.crosstab.num(ct_data))
}

#' @noRd
#' @export
get_anova.crosstab_data_num <- function(ct_data) {
    assert_crosstab_data(ct_data)
    assert_that(
        is.crosstab.grouped(ct_data),
        msg = "Can not perform ANOVA test on ungrouped data"
    )

    new_formula <- stats::as.formula(sprintf(
        "`%s` ~ `%s`",
        var_name(ct_data),
        cohort_name(ct_data)
    ))
    stats::aov(new_formula, data = get_raw_data(ct_data))
}

# CHI-SQUARE
get_chisq <- function(df) {
    assert_that(is.data.frame(df))
    df <- remove_zero_rows(df)

    if (nrow(df) == 0)
        return(NA)
    if (any(sapply(df, function(col) all(col == 0, na.rm = TRUE))))
        return(NA)

    if (chisq_assumptions_met(df)) {
        return(stats::chisq.test(df)[["p.value"]])
    } else {
        return(stats::chisq.test(df, simulate.p.value = TRUE, B = 10000)[["p.value"]])
    }
}

# RAO-SCOTT
get_rao_scott <- function(df, var_col_name, cohort_col_name) {
    assert_that(is.data.frame(df))
    assert_that(ncol(df) == 2)
    assert_that(is.character(var_col_name), is.character(cohort_col_name))
    assert_that(all(c(var_col_name, cohort_col_name) %in% names(df)))
    assert_that(!any(duplicated(c(var_col_name, cohort_col_name))))
    assert_that(is.list(df[[var_col_name]]))
    assert_that(
        length(unique(df[[cohort_col_name]])) >= 2,
        msg = "Can not perform Rao-Scott adjusted chi-square test without two cohorts to compare"
    )
    missing_cohorts <- levels(df[[cohort_col_name]])[sapply(levels(df[[cohort_col_name]]), function(cohort) {
        sum(df[[cohort_col_name]] == cohort) == 0
    })]
    assert_that(
        length(missing_cohorts) == 0,
        msg = sprintf(
            "Cohort column \"%s\" contains factor level(s) \"%s\" but df contains no data points in this cohort.",
            cohort_col_name,
            paste(missing_cohorts, collapse = "\", \"")
        )
    )

    id_col_name <- get_non_matching("id", c(var_col_name, cohort_col_name))

    df[[id_col_name]] <- 1:nrow(df)
    long_df <- df |>
        tidyr::unnest({{var_col_name}}) |>
        dplyr::filter(!is.na({{var_col_name}}), {{var_col_name}} != "")

    # Missing factor levels throw errors
    long_df[[var_col_name]] <- as.character(long_df[[var_col_name]])

    design <- survey::svydesign(
        id = stats::as.formula(paste0("~`", id_col_name, "`")),
        data = long_df,
        weights = ~1
    )

    result <- survey::svychisq(
        stats::as.formula(sprintf("~`%s` + `%s`", var_col_name, cohort_col_name)),
        design,
        statistic = "Chisq"
    )

    p_value <- result[["p.value"]]
    names(p_value) <- NULL
    return(p_value)
}

# ANOVA MARKERS
get_anova_markers <- function(posthoc, cohorts, as_str = FALSE, marker_type = NULL, superscript = FALSE, cutoff = 0.05) {
    UseMethod("get_anova_markers", posthoc)
}

#' @noRd
#' @export
get_anova_markers.crosstab <- function(posthoc, cohorts, as_str = FALSE, marker_type = NULL, superscript = FALSE, cutoff = 0.05) {
    get_anova_markers(data_table(posthoc), cohorts = cohorts, as_str = as_str, marker_type = marker_type, superscript = superscript, cutoff = cutoff)
}

#' @noRd
#' @export
get_anova_markers.crosstab_data <- function(posthoc, cohorts, as_str = FALSE, marker_type = NULL, superscript = FALSE, cutoff = 0.05) {
    get_anova_markers(get_tukey_posthoc(posthoc), cohorts = cohorts, as_str = as_str, marker_type = marker_type, superscript = superscript, cutoff = cutoff)
}

#' @noRd
#' @export
get_anova_markers.posthoc <- function(posthoc, cohorts, as_str = TRUE, marker_type = NULL, superscript = FALSE, cutoff = 0.05) {
    assert_that(inherits(posthoc, "posthoc"))
    assert_that(is.character(cohorts) | is.factor(cohorts))

    if (is.factor(cohorts)) cohorts <- as.character(cohorts)
    if (is.null(marker_type)) marker_type <- "symbol"

    marker_list <- lapply(cohorts, function(cohort) {

        # Find how many markers you need
        cohort_levels <- names(posthoc)
        num_markers <- length(cohort_levels) - 1

        if (!(cohort %in% cohort_levels)) return(NULL)
        if (num_markers == 0 | which(cohort_levels == cohort) == 1) return(NULL)

        markers <- get_markers(num_markers, marker_type, superscript)

        # Only search through the cohorts that come before the one you want
        cohort_markers <- NULL
        for (i in 1:(which(cohort_levels == cohort)-1)) {
            if (posthoc[cohort, cohort_levels[i]] <= cutoff) {
                cohort_markers <- c(cohort_markers, markers[i])
            }
        }

        return(cohort_markers)
    })

    names(marker_list) <- cohorts

    if (as_str) {
        marker_list <- lapply(marker_list, function(x) {
            paste0(x, collapse = "")
        })
    }

    return(marker_list)
}
