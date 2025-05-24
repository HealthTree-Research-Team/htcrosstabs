get_test_df <- function(data_type = "n", gr = FALSE, col_name = "data", group_name = "group", nrows = 20, w_na = FALSE, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    data_type <- tolower(data_type)
    data_type <- match.arg(data_type, c("numerical", "categorical", "multi-answer", "likert"))

    # Generate base data_col
    if (data_type == "numerical") {
        data_col <- sample(1:100, nrows, replace = TRUE)
    } else if (data_type == "categorical") {
        data_col <- sample(c("morning", "afternon", "evening", "night"), nrows, replace = TRUE)
    } else if (data_type == "likert") {
        data_col <- sample(c("strongly agree", "agree", "neither", "disagree", "strongly disagree"), nrows, replace = TRUE)
    } else {
        choice_options <- c("soccer", "tennis", "basketball", "swimming", "golf", "badminton")
        get_sample <- function() {
            num_choices <- sample(1:3, 1)
            sample(choice_options, num_choices, replace = FALSE)
        }
        data_col <- replicate(nrows, get_sample(), simplify = FALSE)
    }

    # Introduce NA and/or NULL values
    spatter_missing <- function(vec, w_na) {
        n <- length(vec)
        missing_indices <- sample(n, size = ceiling(0.2 * n))  # e.g., 20% chance
        for (i in missing_indices) {
            if (w_na) {
                vec[[i]] <- NA
            }
        }
        vec
    }

    data_col <- spatter_missing(data_col, w_na)

    # Wrap data_col and group_col in data.frame
    if (gr) {
        group_col <- sample(c("A", "B", "C", "D"), nrows, replace = TRUE)
        group_col <- spatter_missing(group_col, w_na)
        df <- data.frame(col_name = I(data_col), group_name = group_col, stringsAsFactors = FALSE)
    } else {
        df <- data.frame(col_name = I(data_col), stringsAsFactors = FALSE)
    }

    # Rename columns
    names(df)[names(df) == "col_name"] <- col_name
    names(df)[names(df) == "group_name"] <- group_name

    df
}

test_that("is_crosstab_sub() rejects an assortment of non-crosstab objects",{
    expect_false(is_crosstab_sub(get_test_df()))
    expect_false(is_crosstab_sub(data.frame()))
    expect_false(is_crosstab_sub(numeric()))
    expect_false(is_crosstab_sub(1))
    expect_false(is_crosstab_sub("a"))
    expect_false(is_crosstab_sub(TRUE))
    expect_false(is_crosstab_sub(character(0)))
    expect_false(is_crosstab_sub(NA))
    expect_false(is_crosstab_sub(NULL))
})


# Test on full crosstab objects







