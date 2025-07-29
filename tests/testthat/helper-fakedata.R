categorical_levels <- c("morning", "afternoon", "evening", "night")
likert_levels <- c("strongly agree", "agree", "neither", "disagree", "strongly disagree")
multianswer_levels <- c("soccer", "tennis", "basketball", "swimming", "golf", "badminton")

character_levels <- c("A", "B", "C", "D")
numeric_levels <- c(1, 2, 3, 4)
# likert_map <- c(
#     "strongly agree" = 5,
#     "disagree" = 2,
#     "agree" = 4,
#     "neither" = -10,
#     "strongly disagree" = 1
# )

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

num_test_df <- function(col_name = "variable", group_name = "cohort", gr = TRUE, factorize = TRUE, group_type = "c", nrows = 200, w_na_var = TRUE, w_na_cohort = FALSE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    data_col <- sample(1:100, nrows, replace = TRUE)
    data_col <- spatter_missing(data_col, w_na_var)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na_cohort)
        df <- data.frame(col_name = data_col, group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = data_col, stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name

    if (factorize & gr) {
        df[[group_name]] <- factor(
            df[[group_name]],
            levels = if (is.numeric(df[[group_name]])) numeric_levels else character_levels
        )
    }

    df
}

cat_test_df <- function(col_name = "variable", group_name = "cohort", gr = TRUE, factorize = TRUE, group_type = "c", nrows = 200, w_na_var = TRUE, w_na_cohort = FALSE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    data_col <- sample(categorical_levels, nrows, replace = TRUE)
    data_col <- spatter_missing(data_col, w_na_var)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na_cohort)
        df <- data.frame(col_name = data_col, group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = data_col, stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name

    if (factorize) {
        df[[col_name]] <- factor(df[[col_name]], levels = categorical_levels)
        if (gr) {
            df[[group_name]] <- factor(
                df[[group_name]],
                levels = if (is.numeric(df[[group_name]])) numeric_levels else character_levels
            )
        }
    }

    df
}

lik_test_df <- function(col_name = "variable", group_name = "cohort", gr = TRUE, factorize = TRUE, group_type = "c", nrows = 200, w_na_var = TRUE, w_na_cohort = FALSE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    data_col <- sample(likert_levels, nrows, replace = TRUE)
    data_col <- spatter_missing(data_col, w_na_var)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na_cohort)
        df <- data.frame(col_name = data_col, group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = data_col, stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name

    if (factorize) {
        df[[col_name]] <- factor(df[[col_name]], levels = likert_levels)
        if (gr) {
            df[[group_name]] <- factor(
                df[[group_name]],
                levels = if (is.numeric(df[[group_name]])) numeric_levels else character_levels
            )
        }
    }

    df
}

multi_test_df <- function(col_name = "variable", group_name = "cohort", gr = TRUE, factorize = TRUE, group_type = "c", nrows = 200, w_na_var = TRUE, w_na_cohort = FALSE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    get_sample <- function() {
        num_choices <- sample(1:3, 1)
        sample(multianswer_levels, num_choices, replace = FALSE)
    }

    data_col <- replicate(nrows, get_sample(), simplify = FALSE)
    data_col <- spatter_missing(data_col, w_na_var)

    if (gr) {
        group_col <- generate_group_col(nrows, group_type, w_na_cohort)
        df <- data.frame(col_name = I(data_col), group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = I(data_col), stringsAsFactors = FALSE)
    }

    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name

    if (factorize) {
        df[[col_name]] <- factor(df[[col_name]], levels = multianswer_levels)
        if (gr) {
            df[[group_name]] <- factor(
                df[[group_name]],
                levels = if (is.numeric(df[[group_name]])) numeric_levels else character_levels
            )
        }
    }

    df
}
