categorical_levels <- c("morning", "afternoon", "evening", "night")
likert_levels <- c("strongly agree", "agree", "neither", "disagree", "strongly disagree")
multianswer_levels <- c("soccer", "tennis", "basketball", "swimming", "golf", "badminton")

character_levels <- c("A", "B", "C", "D")
numeric_levels <- c(1, 2, 3, 4)
likert_map <- c(
    "strongly agree" = 5,
    "disagree" = 2,
    "agree" = 4,
    "neither" = -10,
    "strongly disagree" = 1
)

spatter_missing <- function(vec, w_na) {
    n <- length(vec)
    missing_indices <- sample(n, size = ceiling(0.2 * n))
    for (i in missing_indices) {
        if (w_na) vec[[i]] <- NA
    }
    vec
}

generate_group_col <- function(nrows, group_type, w_na) {
    group_type <- tolower(group_type)
    group_type <- match.arg(group_type, c("categorical", "numeric"))

    if (group_type == "numeric")
        group_col <- sample(numeric_levels, nrows, replace = TRUE)
    else
        group_col <- sample(character_levels, nrows, replace = TRUE)
    spatter_missing(group_col, w_na)
}

add_group_col <- function(df, name = "cohort", value = "Response") {
    df[[name]] <- value
    df[[name]] <- factor(df[[name]])
    df
}

get_numeric_test_df <- function(col_name = "variable", group_name = "cohort", gr = FALSE, group_type = "c", nrows = 200, w_na = TRUE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    data_col <- sample(1:100, nrows, replace = TRUE)
    data_col <- spatter_missing(data_col, w_na)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na)
        df <- data.frame(col_name = data_col, group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = data_col, stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name
    df
}

get_categorical_test_df <- function(col_name = "variable", group_name = "cohort", gr = FALSE, group_type = "c", nrows = 200, w_na = TRUE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    data_col <- sample(categorical_levels, nrows, replace = TRUE)
    data_col <- spatter_missing(data_col, w_na)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na)
        df <- data.frame(col_name = data_col, group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = data_col, stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name
    df
}

get_likert_test_df <- function(col_name = "variable", group_name = "cohort", gr = FALSE, group_type = "c", nrows = 200, w_na = TRUE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    data_col <- sample(likert_levels, nrows, replace = TRUE)
    data_col <- spatter_missing(data_col, w_na)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na)
        df <- data.frame(col_name = data_col, group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = data_col, stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name
    df
}

get_multianswer_test_df <- function(col_name = "variable", group_name = "cohort", gr = FALSE, group_type = "c", nrows = 200, w_na = TRUE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    get_sample <- function() {
        num_choices <- sample(1:3, 1)
        sample(multianswer_levels, num_choices, replace = FALSE)
    }

    data_col <- replicate(nrows, get_sample(), simplify = FALSE)
    data_col <- spatter_missing(data_col, w_na)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na)
        df <- data.frame(col_name = I(data_col), group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = I(data_col), stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name
    df
}

factorize_columns <- function(df, colnames = NULL) {
    if (is.null(colnames))
        colnames <- names(df)

    stopifnot(all(colnames %in% names(df)))

    for (col in colnames) {
        if (is.list(df[[col]])) {
            df[[col]] <- lapply(df[[col]], function(x) factor(x))
        } else if (!is.numeric(df[[col]])) {
            df[[col]] <- factor(df[[col]])
        }
    }

    df
}

test_df <- data.frame(
    var = factor(c("a", "b", "a", "c")),
    cohort = factor(c("G1", "G1", "G2", "G2"))
)

test_ct <- new_crosstab_data_cat(
    df = test_df,
    var_col_name = "var",
    var_levels = levels(test_df$var),
    cohort_col_name = "cohort",
    cohort_levels = levels(test_df$cohort)
)
