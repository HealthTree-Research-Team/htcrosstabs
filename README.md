
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

## Creating a Crosstab

A simple crosstab can be created from a one-column data frame.

``` r
head(sports, 5)
#>        sport
#> 1     Hockey
#> 2 Basketball
#> 3   Baseball
#> 4     Soccer
#> 5   Baseball

new_crosstab <- crosstab(sports)
```

A crosstab can group data into cohorts by providing a two column data
frame, one for the data and one for the cohorts. Simply provide the name
of the cohort column when creating it.

``` r
head(sports_by_age, 5)
#>        sport   age
#> 1   Baseball  Teen
#> 2     Soccer Child
#> 3 Basketball  Teen
#> 4     Soccer Adult
#> 5 Basketball Child

new_crosstab <- crosstab(sports_by_age, cohort_col_name = "age")
```

## Crosstab Types

Crosstabs can hold multiple data types: \* Numeric \* Categorical \*
Likert-like \* Multi-response

The data type is automatically determined when you create the crosstab.

### Categorical Crosstabs

Categorical crosstabs are the default data type. Character values,
logical values, and factors will all be counted as categorical.

``` r
head(sports_by_age, 5) # Categorical
#>        sport   age
#> 1   Baseball  Teen
#> 2     Soccer Child
#> 3 Basketball  Teen
#> 4     Soccer Adult
#> 5 Basketball Child

new_crosstab <- crosstab(sports_by_age, "age")
```

### Numeric Crosstabs

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

### Likert-Like Crosstabs

Likert crosstabs are created when categorical data is provided, along
with a named vector to map values to numbers.

``` r
head(licorice_by_age, 5)
#>    opinion   age
#> 1    Likes Child
#> 2    Likes Child
#> 3  Neither  Teen
#> 4  Neither Child
#> 5 Dislikes Adult

likert_map <- c("Likes" = 1, "Neither" = 0, "Dislikes" = -1)

new_crosstab <- crosstab(licorice_by_age, "age", var_map = likert_map)
```

### Multi-Response Crosstabs

Multi-response crosstabs are created by providing a list-column allowing
multiple answers per row.

``` r
head(allergies_by_school, 5)
#>      allergies          school
#> 1 Gluten, .... Cottonwood High
#> 2 Milk, Eg.... Cottonwood High
#> 3 Eggs, Gl....   Brighton High
#> 4         Nuts   Brighton High
#> 5         Milk       Alta High

new_crosstab <- crosstab(allergies_by_school, "school")
```
