
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
new_crosstab <- crosstab_stacked(
    iris,
    cohort_col_name = "Species"
)

kbl(new_crosstab)
```

# More Detailed

## Creating a Crosstab

A simple crosstab can be created from a one-column data frame.

``` r
sports <- sports_by_age[, "sport", drop = FALSE]
head(sports, 5)
#>        sport
#> 1 basketball
#> 2   baseball
#> 3   baseball
#> 4   football
#> 5     tennis

new_crosstab <- crosstab(sports)
```

A crosstab can group data into cohorts by providing a two column data
frame, one for the data and one for the cohorts. Simply provide the name
of the cohort column when creating it.

``` r
head(sports_by_age, 5)
#>        sport   age
#> 1 basketball child
#> 2   baseball child
#> 3   baseball child
#> 4   football child
#> 5     tennis child

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
#>        sport   age
#> 1 basketball child
#> 2   baseball child
#> 3   baseball child
#> 4   football child
#> 5     tennis child

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
head(licorice_by_region, 5)
#>    opinion region
#> 1  neither   west
#> 2 dislikes   west
#> 3 dislikes   west
#> 4    likes   west
#> 5    likes   west

opinion_map <- c("likes" = 1, "neither" = 0, "dislikes" = -1)

new_crosstab <- crosstab(licorice_by_region, "region", var_map = opinion_map)
```

Multi-response crosstabs are created by providing a list-column allowing
multiple answers per row. You can nest columns into the proper format
with `nest_multi_col()`.

``` r
head(allergies_by_school, 5)
#>    allergies        school
#> 1       1, 2 Brighton High
#> 2            Brighton High
#> 3            Brighton High
#> 4    2, 1, 3 Brighton High
#> 5 2, 1, 4, 3 Brighton High

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
can be found at the bottom of this page, or in the man pages by typing
`?cast_crosstab`.

``` r
new_crosstab <- crosstab(licorice_by_region, "region", opinion_map)
is.crosstab.likert(new_crosstab)
#> [1] TRUE

new_crosstab <- as.crosstab.num(new_crosstab)
is.crosstab.likert(new_crosstab)
#> [1] FALSE
```

## Analysis

You can see the data from the crosstabs by using `data_table()`.
Ungrouped data has a dummy cohort column with the name of the final
output column.

``` r
new_crosstab <- crosstab(sports)
data_table(new_crosstab) |> head(5)
#>        sport cohort
#> 1 basketball    All
#> 2   baseball    All
#> 3   baseball    All
#> 4   football    All
#> 5     tennis    All
```

You can extract useful values with the `get_*()` functions. You can get
values like the mean, standard deviation, median, iqr, percentages,
counts, etc. A full list is available at the bottom of this page, or in
the man pages with `?get_values`.

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

There is also a function `join_val()` which lets you quickly and
automatically join different variable tables.

``` r
new_crosstab <- crosstab(length_by_species, "species")

join_val(
    get_complete(new_crosstab),
    get_mean(new_crosstab),
    get_sd(new_crosstab)
)
#>      species complete mean  sd
#> 1        All      150  3.8 1.8
#> 2     setosa       50  1.5 0.2
#> 3 versicolor       50  4.3 0.5
#> 4  virginica       50  5.6 0.6

new_crosstab <- crosstab(sports_by_age, "age")

join_val(
    get_complete(new_crosstab),
    get_count(new_crosstab),
    get_proportion(new_crosstab)
)
#>       age complete      sport count prop
#> 1     All      400 basketball    77 0.19
#> 2     All      400   baseball    81 0.20
#> 3     All      400   football    60 0.15
#> 4     All      400     tennis   102 0.26
#> 5     All      400       golf    80 0.20
#> 6   child      100 basketball    21 0.21
#> 7   child      100   baseball    33 0.33
#> 8   child      100   football    15 0.15
#> 9   child      100     tennis    23 0.23
#> 10  child      100       golf     8 0.08
#> 11   teen      100 basketball    22 0.22
#> 12   teen      100   baseball    12 0.12
#> 13   teen      100   football    21 0.21
#> 14   teen      100     tennis    27 0.27
#> 15   teen      100       golf    18 0.18
#> 16  adult      100 basketball    24 0.24
#> 17  adult      100   baseball    22 0.22
#> 18  adult      100   football     9 0.09
#> 19  adult      100     tennis    20 0.20
#> 20  adult      100       golf    25 0.25
#> 21 senior      100 basketball    10 0.10
#> 22 senior      100   baseball    14 0.14
#> 23 senior      100   football    15 0.15
#> 24 senior      100     tennis    32 0.32
#> 25 senior      100       golf    29 0.29
```

You can also do statistical analysis quickly, like an ANOVA, chi-square,
or Rao-Scott corrected chi-square test with `get_*_p_value()` functions.
Post-hocs can be done with the `get_*_posthoc()` functions. Post-hocs
return a symmetric data frame/matrix of pairwise p-values, which are by
default corrected using the Benjamini-Hochberg correction. You can
change the correction method with `method = ...`, or you can disable
correction completely with `p.adj = FALSE`.

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

## Adding Formatted Rows

You can add rows with pre-formatted data by using the `add_*_row()`
functions. A full list of functions can be found at the bottom of this
page, or in the man pages with `?add_formatted_rows`. You can also
extract the rows separate from the crosstab object with the
`get_*_row()` functions.

The order of the rows and columns is determined by the factor levels of
the input data frame.

``` r
crosstab(length_by_species, "species") |> 
    add_complete_total_row() |> 
    add_mean_sd_row() |> 
    add_med_q1_q3_row()
#>        Description            All         setosa   versicolor      virginica
#> 1 Complete / Total      150 / 150        50 / 50      50 / 50        50 / 50
#> 2        Mean ± SD      3.8 ± 1.8      1.5 ± 0.2    4.3 ± 0.5      5.6 ± 0.6
#> 3     Med (Q1, Q3) 4.3 (1.6, 5.1) 1.5 (1.4, 1.6) 4.3 (4, 4.6) 5.6 (5.1, 5.9)

crosstab(sports_by_age, "age") |> 
    add_complete_total_row() |> 
    add_count_rows()
#>        Description       All     child      teen     adult    senior
#> 1 Complete / Total 400 / 400 100 / 100 100 / 100 100 / 100 100 / 100
#> 2       basketball        77        21        22        24        10
#> 3         baseball        81        33        12        22        14
#> 4         football        60        15        21         9        15
#> 5           tennis       102        23        27        20        32
#> 6             golf        80         8        18        25        29
```

## Adding Pre-Built Tables

Certain types of data have common types of rows you usually want, so we
have preset templates you can call if you want.

``` r
satisfaction_map <- c("very satisfied" = 3, "somewhat satisfied" = 2, "not satisfied" = 1)
crosstab(satisfaction_by_company, "company", satisfaction_map) |> 
    add_likert_table()
#>           Description       All     Apple     Amazon    Google  Microsoft
#> 1    Complete / Total 400 / 400 100 / 100  100 / 100 100 / 100  100 / 100
#> 2           Mean ± SD 2.1 ± 0.8 2.1 ± 0.9  1.9 ± 0.8 2.6 ± 0.5  1.9 ± 0.8
#> 3      very satisfied 155 (39%)  43 (43%)   23 (23%)  62 (62%)   27 (27%)
#> 4  somewhat satisfied 141 (35%)  25 (25%)   39 (39%)  38 (38%)   39 (39%)
#> 5       not satisfied 104 (26%)  32 (32%)   38 (38%)    0 (0%)   34 (34%)
#> 6          Dif. Apple         -         - NS (0.064)   < 0.001 NS (0.314)
#> 7         Dif. Amazon         -         -          -   < 0.001  NS (0.87)
#> 8         Dif. Google         -         -          -         -    < 0.001
#> 9             Overall   < 0.001         -          -         -          -
#> 10         Dif. Apple         -         -      0.012   < 0.001       0.04
#> 11        Dif. Amazon         -         -          -   < 0.001 NS (0.763)
#> 12        Dif. Google         -         -          -         -    < 0.001
#> 13            Overall   < 0.001         -          -         -          -

crosstab(allergies_by_school, "school") |> 
    add_categorical_table()
#>            Description       All Brighton High Cottonwood High Olympus High
#> 1     Complete / Total 400 / 400     100 / 100       100 / 100    100 / 100
#> 2               gluten 141 (35%)      38 (38%)        36 (36%)     32 (32%)
#> 3                 nuts 123 (31%)      34 (34%)        36 (36%)     24 (24%)
#> 4                 eggs  68 (17%)      24 (24%)          9 (9%)     17 (17%)
#> 5                 milk  75 (19%)      17 (17%)        22 (22%)     26 (26%)
#> 6   Dif. Brighton High         -             -           0.007         0.03
#> 7 Dif. Cottonwood High         -             -               -        0.019
#> 8    Dif. Olympus High         -             -               -            -
#> 9              Overall   < 0.001             -               -            -
#>    Alta High
#> 1  100 / 100
#> 2   35 (35%)
#> 3   29 (29%)
#> 4   18 (18%)
#> 5   10 (10%)
#> 6 NS (0.575)
#> 7      0.007
#> 8      0.007
#> 9          -
```

You can also choose `add_default_table()` and it will decide
automatically based on the data you passed in.

``` r
crosstab(length_by_species, "species") |> 
    add_default_table()
#>        Description            All         setosa   versicolor      virginica
#> 1 Complete / Total      150 / 150        50 / 50      50 / 50        50 / 50
#> 2        Mean ± SD      3.8 ± 1.8      1.5 ± 0.2    4.3 ± 0.5      5.6 ± 0.6
#> 3     Med (Q1, Q3) 4.3 (1.6, 5.1) 1.5 (1.4, 1.6) 4.3 (4, 4.6) 5.6 (5.1, 5.9)
#> 4      Dif. setosa              -              -      < 0.001        < 0.001
#> 5  Dif. versicolor              -              -            -        < 0.001
#> 6          Overall        < 0.001              -            -              -

crosstab(sports_by_age, "age") |> 
    add_default_table()
#>         Description       All     child      teen     adult    senior
#> 1  Complete / Total 400 / 400 100 / 100 100 / 100 100 / 100 100 / 100
#> 2        basketball  77 (19%)  21 (21%)  22 (22%)  24 (24%)  10 (10%)
#> 3          baseball  81 (20%)  33 (33%)  12 (12%)  22 (22%)  14 (14%)
#> 4          football  60 (15%)  15 (15%)  21 (21%)    9 (9%)  15 (15%)
#> 5            tennis 102 (26%)  23 (23%)  27 (27%)  20 (20%)  32 (32%)
#> 6              golf  80 (20%)    8 (8%)  18 (18%)  25 (25%)  29 (29%)
#> 7        Dif. child         -         -     0.014     0.024   < 0.001
#> 8         Dif. teen         -         -         -     0.048 NS (0.07)
#> 9        Dif. adult         -         -         -         -     0.025
#> 10          Overall   < 0.001         -         -         -         -
```

## Stacking Crosstabs

If you want stacked crosstabs, there are two ways to do it:

The first way is to make two crosstabs and manually stack them with
`stack_crosstabs()`. You can stack as many as you want.

``` r
ct1 <- iris[, c("Sepal.Length", "Species")] |> 
    crosstab("Species") |> 
    add_default_table(anova = F)

ct2 <- iris[, c("Petal.Length", "Species")] |> 
    crosstab("Species") |> 
    add_default_table(anova = F)

ct3 <- iris[, c("Sepal.Width", "Species")] |> 
    crosstab("Species") |> 
    add_default_table(anova = F)

ct4 <- iris[, c("Petal.Width", "Species")] |> 
    crosstab("Species") |> 
    add_default_table(anova = F)

stack_crosstabs(ct1, ct2, ct3, ct4)
#>         Description            All         setosa     versicolor      virginica
#> 1  Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 2         Mean ± SD      5.8 ± 0.8        5 ± 0.4      5.9 ± 0.5      6.6 ± 0.6
#> 3      Med (Q1, Q3) 5.8 (5.1, 6.4)   5 (4.8, 5.2) 5.9 (5.6, 6.3) 6.5 (6.2, 6.9)
#> 4  Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 5         Mean ± SD      3.8 ± 1.8      1.5 ± 0.2      4.3 ± 0.5      5.6 ± 0.6
#> 6      Med (Q1, Q3) 4.3 (1.6, 5.1) 1.5 (1.4, 1.6)   4.3 (4, 4.6) 5.6 (5.1, 5.9)
#> 7  Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 8         Mean ± SD      3.1 ± 0.4      3.4 ± 0.4      2.8 ± 0.3        3 ± 0.3
#> 9      Med (Q1, Q3)   3 (2.8, 3.3) 3.4 (3.2, 3.7)   2.8 (2.5, 3)   3 (2.8, 3.2)
#> 10 Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 11        Mean ± SD      1.2 ± 0.8      0.2 ± 0.1      1.3 ± 0.2        2 ± 0.3
#> 12     Med (Q1, Q3) 1.3 (0.3, 1.8) 0.2 (0.2, 0.3) 1.3 (1.2, 1.5)   2 (1.8, 2.3)
```

The second is to call `crosstab_stacked()` which runs through a data
frame and automatically applies the default table format.

``` r
crosstab_stacked(iris, "Species", anova = F)
#>         Description            All         setosa     versicolor      virginica
#> 1  Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 2         Mean ± SD      5.8 ± 0.8        5 ± 0.4      5.9 ± 0.5      6.6 ± 0.6
#> 3      Med (Q1, Q3) 5.8 (5.1, 6.4)   5 (4.8, 5.2) 5.9 (5.6, 6.3) 6.5 (6.2, 6.9)
#> 4  Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 5         Mean ± SD      3.1 ± 0.4      3.4 ± 0.4      2.8 ± 0.3        3 ± 0.3
#> 6      Med (Q1, Q3)   3 (2.8, 3.3) 3.4 (3.2, 3.7)   2.8 (2.5, 3)   3 (2.8, 3.2)
#> 7  Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 8         Mean ± SD      3.8 ± 1.8      1.5 ± 0.2      4.3 ± 0.5      5.6 ± 0.6
#> 9      Med (Q1, Q3) 4.3 (1.6, 5.1) 1.5 (1.4, 1.6)   4.3 (4, 4.6) 5.6 (5.1, 5.9)
#> 10 Complete / Total      150 / 150        50 / 50        50 / 50        50 / 50
#> 11        Mean ± SD      1.2 ± 0.8      0.2 ± 0.1      1.3 ± 0.2        2 ± 0.3
#> 12     Med (Q1, Q3) 1.3 (0.3, 1.8) 0.2 (0.2, 0.3) 1.3 (1.2, 1.5)   2 (1.8, 2.3)
```

If you have two likert columns, you can pass multiple maps into
`var_maps` as a list, where the name of the list item is the same as the
column name.

``` r
perception_map <- c(
    "overwhelmingly positive" = 2, 
    "somewhat positive" = 1, 
    "neutral" = 0, 
    "somewhat negative" = -1, 
    "overwhelmingly negative" = -2
)
support_map <- c(
    "very supportive" = 4, 
    "somewhat supportive" = 3, 
    "apathetic" = 2, 
    "somewhat demeaning" = 1, 
    "very demeaning" = 0
)

# Names in map match their respective columns
likert_maps <- list(
    uni_perception = perception_map,
    prof_support = support_map
)

keep_cols <- c("university", "uni_perception", "prof_support")

crosstab_stacked(
    df = students[, keep_cols],
    cohort_col_name = "university",
    var_map = likert_maps,
    anova = F,
    chisq = F
)
#>                Description         All      UCLA  Berkeley  Stanford      Yale
#> 1         Complete / Total 3500 / 3500 500 / 500 500 / 500 500 / 500 500 / 500
#> 2                Mean ± SD   0.8 ± 1.3 0.6 ± 1.5 0.4 ± 1.2 1.2 ± 1.2 1.1 ± 1.1
#> 3  overwhelmingly positive  1343 (38%) 218 (44%)  90 (18%) 289 (58%) 230 (46%)
#> 4        somewhat positive   849 (24%)  68 (14%) 160 (32%)  94 (19%) 160 (32%)
#> 5                  neutral   752 (21%) 109 (22%) 150 (30%)  62 (12%)  57 (11%)
#> 6        somewhat negative    277 (8%)   25 (5%)  51 (10%)   30 (6%)   23 (5%)
#> 7  overwhelmingly negative    279 (8%)  80 (16%)  49 (10%)   25 (5%)   30 (6%)
#> 8         Complete / Total 3500 / 3500 500 / 500 500 / 500 500 / 500 500 / 500
#> 9                Mean ± SD     3 ± 1.2 3.2 ± 1.3 2.8 ± 1.2 3.4 ± 1.1 3.2 ± 1.1
#> 10         very supportive  1714 (49%) 292 (58%) 193 (39%) 332 (66%) 247 (49%)
#> 11     somewhat supportive   836 (24%) 105 (21%) 112 (22%)  79 (16%) 159 (32%)
#> 12               apathetic   466 (13%)   34 (7%) 118 (24%)   42 (8%)   41 (8%)
#> 13      somewhat demeaning    271 (8%)   29 (6%)   47 (9%)   29 (6%)   30 (6%)
#> 14          very demeaning    213 (6%)   40 (8%)   30 (6%)   18 (4%)   23 (5%)
#>          MIT Texas Tech   Alabama
#> 1  500 / 500  500 / 500 500 / 500
#> 2  0.5 ± 1.3  0.7 ± 1.2 0.9 ± 1.1
#> 3  143 (29%)  168 (34%) 205 (41%)
#> 4   99 (20%)  139 (28%) 129 (26%)
#> 5  162 (32%)  111 (22%) 101 (20%)
#> 6    47 (9%)   51 (10%)  50 (10%)
#> 7   49 (10%)    31 (6%)   15 (3%)
#> 8  500 / 500  500 / 500 500 / 500
#> 9  2.7 ± 1.3    3 ± 1.2   3 ± 1.2
#> 10 172 (34%)  245 (49%) 233 (47%)
#> 11 138 (28%)  115 (23%) 128 (26%)
#> 12  89 (18%)   65 (13%)  77 (15%)
#> 13  52 (10%)   52 (10%)   32 (6%)
#> 14  49 (10%)    23 (5%)   30 (6%)
```

## Printable Output

The `kableExtra` package contains a lot of good stuff, so I made a
wrapper for the `kbl()` function which will recognize a crosstab object
and automatically apply formatting such as auto-generated footnotes.

``` r
new_crosstab <- crosstab_stacked(students, "university")

htcrosstabs::kbl(new_crosstab)
```

# Other Utilities

Because the output order of the rows and columns is determined by factor
levels, and because multi-response data is handled in list-columns, and
because list-columns are a pain to factorize, I included wrappers for
`factor()` and `levels()` which include support for list-columns.

``` r
head(allergies_by_school, 5)
#>    allergies        school
#> 1       1, 2 Brighton High
#> 2            Brighton High
#> 3            Brighton High
#> 4    2, 1, 3 Brighton High
#> 5 2, 1, 4, 3 Brighton High

new_allergies <- factor(allergies_by_school$allergies, levels = c("nuts", "eggs", "milk", "gluten"))
levels(new_allergies)
#> [1] "nuts"   "eggs"   "milk"   "gluten"
```

I also included a function `default_var_map()` which will automatically
generate a likert map based on factor levels.

``` r
default_var_map(students$prof_support)
#>     very supportive somewhat supportive           apathetic  somewhat demeaning 
#>                   5                   4                   3                   2 
#>      very demeaning 
#>                   1
```

# Complete Function Index

## Crosstab Creation

``` r
crosstab()         # Create crosstab
as.crosstab()      # Create crosstab (Same as crosstab())
crosstab_stacked() # Create crosstab with rows from multi-column data frame
stack_crosstabs()  # Combine 2+ crosstabs
crosstab_data()    # Create internal data table (you shouldn't need this)
```

## Class attributes

``` r
# GETTERS
var_name()             # The name of the variable column
var()                  # The variable column as a vector
var_levels()           # The factor levels in the variable column
var_map()              # The named numeric vector mapping variable to number (likert)
var_mapped()           # The variable column mapped to its numeric representation
cohort_name()          # The name of the cohort column
cohort()               # The cohort column as a vector
cohort_levels()        # Factor levels in the cohort column
combined_cohort_name() # Name of the combined cohort column in the output
desc_name()            # Name of the leftmost description column in the output
data_table()           # The internal data table containing the original data
get_raw_data()         # The internal data table without combined cohort
index()                # The vector keeping track of row groupings

# SETTERS
set_new_data()         # Replace data table in current crosstab with a new one
`data_table<-`()       # Replace data table in current crosstab with a new one
`var_name<-`()         # Rename the variable column
`var<-`()              # Set new values for the variable column
`var_levels<-`()       # Set new variable factor levels
`var_map<-`()          # Set new numeric values to map the variables to
`cohort_name<-`()      # Rename the cohort column
`cohort<-`()           # Set new values for the cohort column
`cohort_levels<-`()    # Set new cohort factor levels
`index<-`()            # Change the row grouping for the output table
```

## Class Checking

``` r
is.crosstab()                 # Is this a crosstab object? (FALSE if not)
is.crosstab.grouped()         # Is this a grouped crosstab? (FALSE if not)
is.crosstab.categorical()     # Is this a categorical crosstab? (FALSE if not)
is.crosstab.numeric()         # Is this a numeric crosstab? (FALSE if not)
is.crosstab.likert()          # Is this a likert crosstab? (FALSE if not)
is.crosstab.multi()           # Is this a multi-response crosstab? (FALSE if not)
is.crosstab.data()            # Is this an internal crosstab_data table? (FALSE if not)

assert_crosstab()             # Is this a crosstab object? (Errors if not)
assert_crosstab_grouped()     # Is this a grouped crosstab? (Errors if not)
assert_crosstab_categorical() # Is this a categorical crosstab? (Errors if not)
assert_crosstab_numeric()     # Is this a numeric crosstab? (Errors if not)
assert_crosstab_likert()      # Is this a likert crosstab? (Errors if not)
assert_crosstab_multi()       # Is this a multi-response crosstab? (Errors if not)
assert_crosstab_data()        # Is this an internal crosstab_data table? (Errors if not)
```

## Class Casting

``` r
as.crosstab()        # Cast data frame to crosstab
as.crosstab.cat()    # Cast crosstab to categorical
as.crosstab.num()    # Cast crosstab to numeric
as.crosstab.likert() # Cast crosstab to likert
as.crosstab.multi()  # Cast crosstab to multi-response
```

## Get Values

``` r
join_val()           # Quickly join multiple value tables

get_complete()       # Get complete (not NA)
get_total()          # Get total (including NA)
get_complete_total() # Get "complete / total"

get_mean()           # Get mean
get_sd()             # Get standard deviation
get_mean_sd()        # Get "mean +/- sd"

get_med()            # Get median
get_median()         # Get median
get_q1()             # Get first quartile
get_q3()             # Get third quartile
get_q1_q3()          # Get "Q1--Q3"
get_med_q1_q3()      # Get "med (Q1, Q3)"
get_iqr()            # Get IQR
get_iqr_q3_q1()      # Get "IQR (Q3-Q1)"

get_count()          # Get counts
get_prop()           # Get proportions (out of complete)
get_proportion()     # Get proportions (out of complete)
get_count_prop()     # Get "count (proportion)"
get_percent()        # Get percent
get_count_percent()  # Get "count (percent%)"
```

## Get Stats

``` r
get_anova_p_value()     # Get overall ANOVA p-value
get_anova_posthoc()     # Get Tukey post-hoc p-values
get_tukey_posthoc()     # Get Tukey post-hoc p-values

get_chisq_p_value()     # Get overall chi-square p-value
get_chisq_posthoc()     # Get chi-square post-hoc p_values

get_rao_scott_p_value() # Get overall Rao-Scott p-value
get_rao_scott_posthoc() # Get Rao-Scott post-hoc p-values
```

## Get/Add Formatted Rows

``` r
add_rows()               # Add custom rows to the output table

get_complete_row()       # Get formatted complete row
add_complete_row()       # Get formatted complete row
get_total_row()          # Get formatted total row
add_total_row()          # Get formatted total row
get_complete_total_row() # Get formatted "complete / total row"
add_complete_total_row() # Get formatted "complete / total row"

get_mean_row()           # Get formatted mean row
add_mean_row()           # Get formatted mean row
get_sd_row()             # Get formatted standard deviation row
add_sd_row()             # Get formatted standard deviation row
get_mean_sd_row()        # Get formatted "mean +/- sd" row
add_mean_sd_row()        # Get formatted "mean +/- sd" row

get_med_row()            # Get formatted median row
add_med_row()            # Get formatted median row
get_median_row()         # Get formatted median row
add_median_row()         # Get formatted median row
get_q1_row()             # Get formatted Q1 row
add_q1_row()             # Get formatted Q1 row
get_q3_row()             # Get formatted Q3 row
add_q3_row()             # Get formatted Q3 row
get_q1_q3_row()          # Get formatted "Q1--Q3" row
add_q1_q3_row()          # Get formatted "Q1--Q3" row
get_med_q1_q3_row()      # Get formatted "med (Q1, Q3)" row
add_med_q1_q3_row()      # Get formatted "med (Q1, Q3)" row
get_iqr_row()            # Get formatted IQR row
add_iqr_row()            # Get formatted IQR row
get_iqr_q3_q1_row()      # Get formatted "IQR (Q3-Q1)" row
add_iqr_q3_q1_row()      # Get formatted "IQR (Q3-Q1)" row

get_count_rows()         # Get formatted count rows
add_count_rows()         # Get formatted count rows
get_prop_rows()          # Get formatted proportion rows
add_prop_rows()          # Get formatted proportion rows
get_proportion_rows()    # Get formatted proportion rows
add_proportion_rows()    # Get formatted proportion rows
get_count_prop_rows()    # Get formatted "count (proportion)" rows
add_count_prop_rows()    # Get formatted "count (proportion)" rows
get_percent_rows()       # Get formatted percent rows
add_percent_rows()       # Get formatted percent rows
get_count_percent_rows() # Get formatted "count (percent%)" rows
add_count_percent_rows() # Get formatted "count (percent%)" rows

get_anova_rows()         # Get formatted ANOVA rows
add_anova_rows()         # Get formatted ANOVA rows
get_chisq_rows()         # Get formatted chi-square rows
add_chisq_rows()         # Get formatted chi-square rows
get_rao_scott_rows()     # Get formatted Rao-Scott rows
add_rao_scott_rows()     # Get formatted Rao-Scott rows
```

## Add Pre-Built Tables

``` r
add_default_table()     # Guess which table based on the data type

add_categorical_table() # Add default categorical table
add_numeric_table()     # Add default numeric table
add_likert_table()      # Add default likert table
```

## Kable Extensions

``` r
kbl()             # Wrapper for kableExtra::kbl(), automatically applies formatting

apply_footnotes() # Manually apply footnotes (automatically called by kbl())
apply_pack_rows() # Manually apply row groups (automatically called by kbl())
apply_table_sep() # Manually apply lines between tables (automatically called by kbl())
```

## Miscellaneous Utilities

``` r
factor()          # Wrapper for base::factor(), supports lists
levels()          # Wrapper for base::levels(), supports lists
`levels<-`()      # Wrapper for base::`levels<-`(), supports lists
is.factorlist()   # Is this a list of factor objects?

default_var_map() # Create a default likert map based on factor levels

nest_multi_col()  # Nest column into proper multi-response form
to_long()         # Pivot data from formatted row format to long format
to_wide()         # Pivot data from 3-column long (description, cohort, variable) to formatted row
```
