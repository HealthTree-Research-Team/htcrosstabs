CROSSTAB_SUB <- "crosstab_sub"
CROSSTAB_SUB_MUL <- "crosstab_sub_mul"
CROSSTAB_SUB_GR <- "crosstab_sub_gr"

is_crosstab_sub <- function(obj, mul = F, gr = F) {
    classes <- c("CROSSTAB_SUB")
    if (mul) classes <- c(classes, "CROSSTAB_SUB_MUL")
    if (gr)  classes <- c(classes, "CROSSTAB_SUB_GR")
    all(inherits(obj, classes, which = T))
}

new_crosstab_sub <- function(data, field, levels) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(field))
    stopifnot(is.character(levels))

    structure(
        list(
            data,
            levels,
            field,
            output_table = data.frame()
        ),
        class = CROSSTAB_SUB
    )
}

new_crosstab_sub_mul <- function(data, field, levels) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(field))
    stopifnot(is.character(levels))

    structure(
        list(
            data,
            levels,
            field,
            output_table = data.frame()
        ),
        class = c(CROSSTAB_SUB, CROSSTAB_SUB_MUL)
    )
}

new_crosstab_sub_gr <- function(data, field, levels, group_by, group_levels) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(field))
    stopifnot(is.character(levels))
    stopifnot(is.character(group_by))
    stopifnot(is.character(group_levels))

    structure(
        list(
            data,
            levels,
            field,
            group_by,
            group_levels,
            output_table = data.frame()
        ),
        class = c(CROSSTAB_SUB, CROSSTAB_SUB_GR)
    )
}

new_crosstab_sub_mul_gr <- function(data, field, levels, group_by, group_levels) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(field))
    stopifnot(is.character(levels))
    stopifnot(is.character(group_by))
    stopifnot(is.character(group_levels))

    structure(
        list(
            data,
            levels,
            field,
            group_by,
            group_levels,
            output_table = data.frame()
        ),
        class = c(CROSSTAB_SUB, CROSSTAB_SUB_MUL, CROSSTAB_SUB_GR)
    )
}

validate_crosstab_sub <- function(obj) {

}

validate_crosstab_sub_mul <- function(obj) {

}

validate_crosstab_sub_gr <- function(obj) {

}

validate_crosstab_sub_mul_gr <- function(obj) {

}
