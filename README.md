
<!-- README.md is generated from README.Rmd. Please edit that file -->

# htcrosstabs

<!-- badges: start -->
<!-- badges: end -->

The goal of htcrosstabs is to easily create printable crosstabs from raw
data frames.

## Installation

You can install the development version of htcrosstabs like so:

``` r
devtools::install_github("HealthTree-Research-Team/htcrosstabs")
```

And you can load it like so:

``` r
library(htcrosstabs)
```

# Quick Start

The fast way to create crosstabs is to pass your data frame into
`crosstab_stacked()`, telling it if you have a cohort column. Then pass
the object into `kbl()`; not the `kableExtra` one, the one that comes
with `htcrosstabs`. It works exactly the same way, but applies some
automatic formatting like adding auto-generated footnotes and row
groupings.

``` r
test_data <- iris
new_crosstab <- crosstab_stacked(cohort_col_name = "Species")

# Output
kbl(new_crosstab)
```

# More Detailed

## Creating a Crosstab

A simple crosstab can be created from a one-column data frame.

``` r
head(sports, 5)
#>        sport
#> 1     Soccer
#> 2 Basketball
#> 3   Baseball
#> 4     Hockey
#> 5 Basketball

new_crosstab <- crosstab(sports)
```

A crosstab can group data into cohorts by providing a two column data
frame, one for the data and one for the cohorts. Simply provide the name
of the cohort column when creating it.

``` r
head(sports_by_age, 5)
#>      sport   age
#> 1 Baseball  Teen
#> 2 Football  Teen
#> 3 Baseball Adult
#> 4 Football  Teen
#> 5   Hockey Child

new_crosstab <- crosstab(sports_by_age, cohort_col_name = "age")
```

## Crosstab Types

Crosstabs can hold multiple data types: categorical, numeric,
Likert-like, and multi-response. The data type is automatically
determined when you create the crosstab.

Categorical crosstabs are the default data type. Character values,
logical values, and factors will all be counted as categorical.

``` r
head(sports_by_age, 5) # Categorical
#>      sport   age
#> 1 Baseball  Teen
#> 2 Football  Teen
#> 3 Baseball Adult
#> 4 Football  Teen
#> 5   Hockey Child

new_crosstab <- crosstab(sports_by_age, "age")
```

Numeric crosstabs are the default when the data is numbers.

``` r
head(length_by_species, 5)
#>   petal length species
#> 1          1.4  setosa
#> 2          1.4  setosa
#> 3          1.3  setosa
#> 4          1.5  setosa
#> 5          1.4  setosa

new_crosstab <- crosstab(length_by_species, "species")
```

Likert crosstabs are created when categorical data is provided, along
with a named vector to map values to numbers.

``` r
head(licorice_by_age, 5)
#>    opinion   age
#> 1 Dislikes Adult
#> 2  Neither Adult
#> 3  Neither Adult
#> 4  Neither Adult
#> 5  Neither Adult

likert_map <- c("Likes" = 1, "Neither" = 0, "Dislikes" = -1)

new_crosstab <- crosstab(licorice_by_age, "age", var_map = likert_map)
```

Multi-response crosstabs are created by providing a list-column allowing
multiple answers per row.

``` r
head(allergies_by_school, 5)
#>      allergies        school
#> 1 Nuts, Eg....     Alta High
#> 2         Milk  Olympus High
#> 3 Milk, Nu....  Olympus High
#> 4   Nuts, Milk Brighton High
#> 5 Eggs, Gl.... Brighton High

new_crosstab <- crosstab(allergies_by_school, "school")
```

## Checking and Casting Data Types

You can check the data type and whether it’s grouped or not with
`is.crosstab.*()` functions.

``` r
new_crosstab <- crosstab(length_by_species, "species")

is.crosstab.grouped(new_crosstab)
#> [1] TRUE
is.crosstab.categorical(new_crosstab)
#> [1] FALSE
is.crosstab.numeric(new_crosstab)
#> [1] TRUE
is.crosstab.likert(new_crosstab)
#> [1] FALSE
is.crosstab.multi(new_crosstab)
#> [1] FALSE
```

You can also, if needed, cast one data type to another using the
`as.crosstab.*()` functions. The details on how the conversion is done
can be found in the man pages by typing `?cast_crosstab`.

``` r
new_crosstab <- crosstab(licorice_by_age, "age", likert_map)
is.crosstab.likert(new_crosstab)
#> [1] TRUE

new_crosstab <- as.crosstab.num(new_crosstab)
is.crosstab.likert(new_crosstab)
#> [1] FALSE
```

## Analysis

You can see the data from the crosstabs by using `data_table()`.
Ungrouped data has a dummy cohort column added with the name of the
column in the output.

``` r
new_crosstab <- crosstab(sports)
data_table(new_crosstab) |> head(5)
#>        sport cohort
#> 1     Soccer    All
#> 2 Basketball    All
#> 3   Baseball    All
#> 4     Hockey    All
#> 5 Basketball    All
```

You can extract useful values with the `get_*()` functions. You can get
values like the mean, standard deviation, median, iqr, percentages,
counts, etc. A full list is available in the man pages with
`?get_values`.

``` r
new_crosstab <- crosstab(length_by_species, "species")

get_mean(new_crosstab)
#>      species mean
#> 1        All  3.8
#> 2     setosa  1.5
#> 3 versicolor  4.3
#> 4  virginica  5.6

get_complete_total(new_crosstab)
#>      species comp_total
#> 1        All  150 / 150
#> 2     setosa    50 / 50
#> 3 versicolor    50 / 50
#> 4  virginica    50 / 50
```

You can also do statistical analysis quickly, like an ANOVA, chi-square,
or Rao-Scott corrected chi-square test with `get_*_p_value()` functions.
Post hocs return a symmetric data frame/matrix of pairwise p-values,
which are by default corrected using the Benjamini-Hochberg correction.
You can change the correction method with `method = ...`, or you can
disable correction completely with `p.adj = FALSE`.

``` r
new_crosstab <- crosstab(length_by_species, "species")

get_anova_p_value(new_crosstab)
#> [1] 2.856777e-91

get_tukey_posthoc(new_crosstab)
#>                  setosa   versicolor    virginica
#> setosa     1.000000e+00 2.997602e-15 2.997602e-15
#> versicolor 2.997602e-15 1.000000e+00 2.997602e-15
#> virginica  2.997602e-15 2.997602e-15 1.000000e+00
```
