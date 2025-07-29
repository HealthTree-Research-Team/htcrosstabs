test_that("add_footnote() adds the footnotes to the crosstab object",{
    test_ct <- crosstab(cat_test_df(seed = 1234), "cohort")

    expect_silent(add_footnote(test_ct, "chisq", "This is a test footnote"))
    result <- add_footnote(test_ct, "chisq", "This is a test footnote")

    expect_true(!is.null(footnotes(result)))
    expect_equal(footnotes(result), list(
        chisq = "This is a test footnote"
    ))

    expect_silent(add_footnote(test_ct, "likert", "This is a test footnote"))
    result <- add_footnote(test_ct, "likert", "This is a test footnote")

    expect_equal(footnotes(result), list(
        likert = "This is a test footnote"
    ))

    result <- test_ct |>
        add_footnote("chisq", "This is a test footnote") |>
        add_footnote("likert", "This is a test footnote")

    expect_equal(footnotes(result), list(
        chisq = "This is a test footnote",
        likert = "This is a test footnote"
    ))
})

test_that("anova_marker_footnotes() add the marker footnotes to the crosstab",{
    test_ct <- crosstab(num_test_df(seed = 134), "cohort")

    expect_silent(add_anova_marker_footnotes(test_ct, NULL, 0.05))
    result <- add_anova_marker_footnotes(test_ct, NULL, 0.05)

    expect_true(!is.null(footnotes(result)))
    expect_equal(
        footnotes(result),
        list(
            anova = "Numeric data has p-values calculated by ANOVA test with Tukey post-hoc.",
            symbol = c("Significantly different than \"A\"", "Significantly different than \"B\"", "Significantly different than \"C\"")
        )
    )

    expect_silent(add_anova_marker_footnotes(test_ct, "symbol", 0.05))
    result <- add_anova_marker_footnotes(test_ct, "symbol", 0.05)

    expect_true(!is.null(footnotes(result)))
    expect_equal(
        footnotes(result),
        list(
            anova = "Numeric data has p-values calculated by ANOVA test with Tukey post-hoc.",
            symbol = c("Significantly different than \"A\"", "Significantly different than \"B\"", "Significantly different than \"C\"")
        )
    )

    expect_silent(add_anova_marker_footnotes(test_ct, "number", 0.05))
    result <- add_anova_marker_footnotes(test_ct, "number", 0.05)

    expect_true(!is.null(footnotes(result)))
    expect_equal(
        footnotes(result),
        list(
            anova = "Numeric data has p-values calculated by ANOVA test with Tukey post-hoc.",
            number = c("Significantly different than \"A\"", "Significantly different than \"B\"", "Significantly different than \"C\"")
        )
    )

    expect_silent(add_anova_marker_footnotes(test_ct, "alphabet", 0.05))
    result <- add_anova_marker_footnotes(test_ct, "alphabet", 0.05)

    expect_true(!is.null(footnotes(result)))
    expect_equal(
        footnotes(result),
        list(
            anova = "Numeric data has p-values calculated by ANOVA test with Tukey post-hoc.",
            alphabet = c("Significantly different than \"A\"", "Significantly different than \"B\"", "Significantly different than \"C\"")
        )
    )

    expect_false(identical(
        footnotes(add_anova_marker_footnotes(test_ct, "symbol", 0.05)),
        footnotes(add_anova_marker_footnotes(test_ct, "symbol", 0.03))
    ))
})

test_that("add_anova_row_footnotes() adds the proper footnotes",{
    test_ct <- crosstab(num_test_df(seed = 1234), "cohort")

    expect_silent(footnotes(add_anova_row_footnotes(test_ct, 0.05)))
    result <- footnotes(add_anova_row_footnotes(test_ct, 0.05))

    expect_equal(result, list(
        anova = "Numeric data has p-values calculated by ANOVA test with Tukey post-hoc."
    ))

    expect_silent(footnotes(add_anova_row_footnotes(test_ct, 0.03)))
    result <- footnotes(add_anova_row_footnotes(test_ct, 0.03))

    expect_equal(result, list(
        anova = "Numeric data has p-values calculated by ANOVA test (cutoff for significance set at 0.03)."
    ))
})

test_that("add_chisq_row_footnotes() adds the proper footnotes",{
    test_ct <- crosstab(cat_test_df(seed = 1234), "cohort")

    expect_silent(footnotes(add_chisq_row_footnotes(test_ct, T, "BH", 0.05)))
    result <- footnotes(add_chisq_row_footnotes(test_ct, T, "BH", 0.05))

    expect_equal(result, list(
        chisq = "Categorical data has p-values calculated by Pearson chi-squared test with Benjamini-Hochberg adjustment."
    ))

    expect_silent(footnotes(add_chisq_row_footnotes(test_ct, F, "BH", 0.05)))
    result <- footnotes(add_chisq_row_footnotes(test_ct, F, "BH", 0.05))

    expect_equal(result, list(
        chisq = "Categorical data has p-values calculated by Pearson chi-squared test."
    ))

    expect_silent(footnotes(add_chisq_row_footnotes(test_ct, T, "hommel", 0.05)))
    result <- footnotes(add_chisq_row_footnotes(test_ct, T, "hommel", 0.05))

    expect_equal(result, list(
        chisq = "Categorical data has p-values calculated by Pearson chi-squared test with Hommel adjustment."
    ))

    expect_silent(footnotes(add_chisq_row_footnotes(test_ct, T, "hommel", 0.03)))
    result <- footnotes(add_chisq_row_footnotes(test_ct, T, "hommel", 0.03))

    expect_equal(result, list(
        chisq = "Categorical data has p-values calculated by Pearson chi-squared test with Hommel adjustment (cutoff for significance set at 0.03)."
    ))
})

test_that("add_rao_scott_row_footnotes() adds the proper footnotes",{
    test_ct <- crosstab(cat_test_df(seed = 1234), "cohort")

    expect_silent(footnotes(add_rao_scott_row_footnotes(test_ct, T, "BH", 0.05)))
    result <- footnotes(add_rao_scott_row_footnotes(test_ct, T, "BH", 0.05))

    expect_equal(result, list(
        `rao-scott` = "Multi-response data has p-values calculated by chi-squared test with Rao-Scott correction and Benjamini-Hochberg adjustment."
    ))

    expect_silent(footnotes(add_rao_scott_row_footnotes(test_ct, F, "BH", 0.05)))
    result <- footnotes(add_rao_scott_row_footnotes(test_ct, F, "BH", 0.05))

    expect_equal(result, list(
        `rao-scott` = "Multi-response data has p-values calculated by chi-squared test with Rao-Scott correction."
    ))

    expect_silent(footnotes(add_rao_scott_row_footnotes(test_ct, T, "hommel", 0.05)))
    result <- footnotes(add_rao_scott_row_footnotes(test_ct, T, "hommel", 0.05))

    expect_equal(result, list(
        `rao-scott` = "Multi-response data has p-values calculated by chi-squared test with Rao-Scott correction and Hommel adjustment."
    ))

    expect_silent(footnotes(add_rao_scott_row_footnotes(test_ct, T, "hommel", 0.03)))
    result <- footnotes(add_rao_scott_row_footnotes(test_ct, T, "hommel", 0.03))

    expect_equal(result, list(
        `rao-scott` = "Multi-response data has p-values calculated by chi-squared test with Rao-Scott correction and Hommel adjustment (cutoff for significance set at 0.03)."
    ))
})

test_that("add_likert_map_footnotes() adds the proper footnotes",{
    test_ct <- crosstab(cat_test_df(seed = 1234), "cohort")
    test_map <- default_var_map(data_table(test_ct)[["variable"]])

    expect_silent(footnotes(add_likert_map_footnotes(test_ct, test_map)))
    result <- footnotes(add_likert_map_footnotes(test_ct, test_map))

    expect_equal(result, list(
        likert = "variable: \"morning\" = 4, \"afternoon\" = 3, \"evening\" = 2, \"night\" = 1."
    ))
})
