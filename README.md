
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

## Example Data

If you’d like to run the code you see here, here is some example data:

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
multiple answers per row.

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
can be found in the man pages by typing `?cast_crosstab`.

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

## Adding Formatted Rows

You can add rows with pre-formatted data by using the `add_*_row()`
functions. A full list of functions can be found in the man pages with
`?add_formatted_rows`. You can also extract the rows separate from the
crosstab object with the `get_*_row()` functions.

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

crosstab_stacked(students, "university", anova = F, chisq = F)
#>                  Description                  All                 UCLA
#> 1           Complete / Total          3500 / 3500            500 / 500
#> 2                       TRUE           1822 (52%)            377 (75%)
#> 3                      FALSE           1678 (48%)            123 (25%)
#> 4           Complete / Total          3500 / 3500            500 / 500
#> 5                  Mean ± SD          179.5 ± 6.2          181.8 ± 5.3
#> 6               Med (Q1, Q3) 179.6 (175.2, 183.8) 181.8 (178.1, 185.3)
#> 7           Complete / Total          3500 / 3500            500 / 500
#> 8                  Mean ± SD               13 ± 2           12.9 ± 2.1
#> 9               Med (Q1, Q3)          13 (12, 14)          13 (11, 14)
#> 10          Complete / Total          3500 / 3500            500 / 500
#> 11 math and physical science            953 (27%)             98 (20%)
#> 12              life science            616 (18%)             75 (15%)
#> 13                  business            668 (19%)            109 (22%)
#> 14                humanities            703 (20%)            111 (22%)
#> 15                psychology            560 (16%)            107 (21%)
#> 16          Complete / Total          3500 / 3500            500 / 500
#> 17   overwhelmingly positive           1343 (38%)            218 (44%)
#> 18         somewhat positive            849 (24%)             68 (14%)
#> 19                   neutral            752 (21%)            109 (22%)
#> 20         somewhat negative             277 (8%)              25 (5%)
#> 21   overwhelmingly negative             279 (8%)             80 (16%)
#> 22          Complete / Total          3500 / 3500            500 / 500
#> 23           very supportive           1714 (49%)            292 (58%)
#> 24       somewhat supportive            836 (24%)            105 (21%)
#> 25                 apathetic            466 (13%)              34 (7%)
#> 26        somewhat demeaning             271 (8%)              29 (6%)
#> 27            very demeaning             213 (6%)              40 (8%)
#> 28          Complete / Total          3500 / 3500            500 / 500
#> 29             Panda Express           2135 (61%)            318 (64%)
#> 30                McDonald's           1641 (47%)            223 (45%)
#> 31              Panera Bread           1937 (55%)            337 (67%)
#> 32                Chik-fil-A           1655 (47%)            181 (36%)
#>                Berkeley             Stanford                 Yale
#> 1             500 / 500            500 / 500            500 / 500
#> 2             209 (42%)            308 (62%)            154 (31%)
#> 3             291 (58%)            192 (38%)            346 (69%)
#> 4             500 / 500            500 / 500            500 / 500
#> 5           177.1 ± 5.5          179.9 ± 5.3          176.6 ± 5.4
#> 6  177.1 (172.9, 180.9) 180.1 (175.9, 183.5) 176.7 (173.2, 180.4)
#> 7             500 / 500            500 / 500            500 / 500
#> 8                13 ± 2             13 ± 1.9             13.1 ± 2
#> 9           13 (12, 14)          13 (12, 14)          13 (12, 14)
#> 10            500 / 500            500 / 500            500 / 500
#> 11            146 (29%)            127 (25%)             70 (14%)
#> 12            116 (23%)            100 (20%)             65 (13%)
#> 13             72 (14%)             93 (19%)             51 (10%)
#> 14            103 (21%)            122 (24%)            177 (35%)
#> 15             63 (13%)             58 (12%)            137 (27%)
#> 16            500 / 500            500 / 500            500 / 500
#> 17             90 (18%)            289 (58%)            230 (46%)
#> 18            160 (32%)             94 (19%)            160 (32%)
#> 19            150 (30%)             62 (12%)             57 (11%)
#> 20             51 (10%)              30 (6%)              23 (5%)
#> 21             49 (10%)              25 (5%)              30 (6%)
#> 22            500 / 500            500 / 500            500 / 500
#> 23            193 (39%)            332 (66%)            247 (49%)
#> 24            112 (22%)             79 (16%)            159 (32%)
#> 25            118 (24%)              42 (8%)              41 (8%)
#> 26              47 (9%)              29 (6%)              30 (6%)
#> 27              30 (6%)              18 (4%)              23 (5%)
#> 28            500 / 500            500 / 500            500 / 500
#> 29            260 (52%)            353 (71%)            301 (60%)
#> 30            241 (48%)            172 (34%)            226 (45%)
#> 31            273 (55%)            297 (59%)            315 (63%)
#> 32            273 (55%)            229 (46%)            237 (47%)
#>                   MIT           Texas Tech            Alabama
#> 1           500 / 500            500 / 500          500 / 500
#> 2            98 (20%)            328 (66%)          348 (70%)
#> 3           402 (80%)            172 (34%)          152 (30%)
#> 4           500 / 500            500 / 500          500 / 500
#> 5           175 ± 5.3          182.5 ± 5.6        183.4 ± 5.1
#> 6  175 (171.2, 178.5) 182.7 (178.8, 186.5) 183.4 (180, 187.1)
#> 7           500 / 500            500 / 500          500 / 500
#> 8          12.9 ± 2.1             13.1 ± 2             13 ± 2
#> 9         13 (11, 14)          13 (12, 14)        13 (12, 14)
#> 10          500 / 500            500 / 500          500 / 500
#> 11          270 (54%)            129 (26%)          113 (23%)
#> 12           99 (20%)            104 (21%)           57 (11%)
#> 13           59 (12%)            123 (25%)          161 (32%)
#> 14            28 (6%)             67 (13%)           95 (19%)
#> 15            44 (9%)             77 (15%)           74 (15%)
#> 16          500 / 500            500 / 500          500 / 500
#> 17          143 (29%)            168 (34%)          205 (41%)
#> 18           99 (20%)            139 (28%)          129 (26%)
#> 19          162 (32%)            111 (22%)          101 (20%)
#> 20            47 (9%)             51 (10%)           50 (10%)
#> 21           49 (10%)              31 (6%)            15 (3%)
#> 22          500 / 500            500 / 500          500 / 500
#> 23          172 (34%)            245 (49%)          233 (47%)
#> 24          138 (28%)            115 (23%)          128 (26%)
#> 25           89 (18%)             65 (13%)           77 (15%)
#> 26           52 (10%)             52 (10%)            32 (6%)
#> 27           49 (10%)              23 (5%)            30 (6%)
#> 28          500 / 500            500 / 500          500 / 500
#> 29          328 (66%)            305 (61%)          270 (54%)
#> 30          228 (46%)            274 (55%)          277 (55%)
#> 31          284 (57%)            212 (42%)          219 (44%)
#> 32          198 (40%)            257 (51%)          280 (56%)
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
