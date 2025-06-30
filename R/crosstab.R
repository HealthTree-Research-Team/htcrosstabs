# IMPORTS ####
#' @import assertthat

# CONSTANTS ####
CT_CLASS <- "crosstab"

# CONSTRUCTORS ####
new_crosstab <- function(df, cohort_col_name = NULL, likert_map = NULL, default_cohort = "Response") {
    assert_that(is.data.frame(df))
    if (!is.null(cohort_col_name))
        assert_that(is.character(cohort_col_name))
    if (!is.null(likert_map))
        assert_that(
            is.numeric(likert_map),
            !is.null(names(likert_map)),
            msg = "likert_map must be a named vector of numeric values"
        )
    assert_that(is.character(default_cohort))

    data <- crosstab_data(
        df = df,
        cohort_col_name = cohort_col_name,
        likert_map = likert_map,
        default_cohort = default_cohort
    )

    structure(
        data.frame(), # Output table
        data = data   # crosstab_data
    )
}

# VALIDATORS ####
validate_crosstab <- function(ct) {
    assert_that(inherits(ct, CT_CLASS))
    assert_that(has_attr(ct, "data"))
    data <- attr(ct, "data")
    validate_crosstab_data(data)

    return(TRUE)
}

# HELPERS ####
#' @export
crosstab <- function(df, cohort_col_name = NULL, likert_map = NULL, default_cohort = "Response") {
    ct <- new_crosstab(
        df = df,
        cohort_col_name = cohort_col_name,
        likert_map = likert_map,
        default_cohort = default_cohort
    )

    validate_crosstab(ct)
    return(ct)
}

# GETTERS ####
#' @export
data <- function(ct) {
    assert_that(has_attr(ct, "data"))
    attr(ct, "data")
}

# SETTERS ####
`data<-` <- function(ct, value) {
    assert_that(inherits(ct, CT_CLASS))
    assert_that(inherits(value, CT_DATA_CLASS))
    attr(ct, "data") <- value
    return(ct)
}
