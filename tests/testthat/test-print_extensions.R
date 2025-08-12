test_that("print.crosstab() works with grouped data", {
    expect_output(print.crosstab(crosstab(sports_by_age, "age")))
    expect_output(print.crosstab(crosstab(length_by_species, "species")))
    expect_output(print.crosstab(crosstab(licorice_by_region, "region", default_var_map(licorice_by_region$opinion))))
    expect_output(print.crosstab(crosstab(allergies_by_school, "school")))
})

test_that("print() calls print.crosstab() on grouped data", {
    expect_equal(
        capture.output(print.crosstab(crosstab(sports_by_age, "age"))),
        capture.output(print(crosstab(sports_by_age, "age")))
    )
    expect_equal(
        capture.output(print.crosstab(crosstab(length_by_species, "species"))),
        capture.output(print(crosstab(length_by_species, "species")))
    )
    expect_equal(
        capture.output(print.crosstab(crosstab(licorice_by_region, "region", default_var_map(licorice_by_region$opinion)))),
        capture.output(print(crosstab(licorice_by_region, "region", default_var_map(licorice_by_region$opinion))))
    )
    expect_equal(
        capture.output(print.crosstab(crosstab(allergies_by_school, "school"))),
        capture.output(print(crosstab(allergies_by_school, "school")))
    )
})

test_that("print.crosstab() works with ungrouped data", {
    expect_output(print.crosstab(crosstab(sports_by_age$sport)))
    expect_output(print.crosstab(crosstab(length_by_species$`petal length`)))
    expect_output(print.crosstab(crosstab(licorice_by_region$opinion, var_map = default_var_map(licorice_by_region$opinion))))
    expect_output(print.crosstab(crosstab(allergies_by_school$allergies)))
})

test_that("print() calls print.crosstab() with ungrouped data", {
    expect_equal(
        capture.output(print.crosstab(crosstab(sports_by_age$sport))),
        capture.output(print(crosstab(sports_by_age$sport)))
    )
    expect_equal(
        capture.output(print.crosstab(crosstab(length_by_species$`petal length`))),
        capture.output(print(crosstab(length_by_species$`petal length`)))
    )
    expect_equal(
        capture.output(print.crosstab(crosstab(licorice_by_region$opinion, var_map = default_var_map(licorice_by_region$opinion)))),
        capture.output(print(crosstab(licorice_by_region$opinion, var_map = default_var_map(licorice_by_region$opinion))))
    )
    expect_equal(
        capture.output(print.crosstab(crosstab(allergies_by_school$allergies))),
        capture.output(print(crosstab(allergies_by_school$allergies)))
    )
})

test_that("print.crosstab() works with stacked crosstabs", {
    expect_output(print.crosstab(crosstab_stacked(iris, "Species")))

    map1 <- default_var_map(students$uni_perception)
    map2 <- default_var_map(students$prof_support)

    var_maps <- list(
        uni_perception = map1,
        prof_support = map2
    )

    expect_output(print.crosstab(crosstab_stacked(students, "university", var_map = var_maps)))
})

test_that("print() calls print.crosstab() with stacked crosstabs", {
    expect_equal(
        capture.output(print.crosstab(crosstab_stacked(iris, "Species"))),
        capture.output(print(crosstab_stacked(iris, "Species")))
    )

    map1 <- default_var_map(students$uni_perception)
    map2 <- default_var_map(students$prof_support)

    var_maps <- list(
        uni_perception = map1,
        prof_support = map2
    )

    expect_equal(
        capture.output(print.crosstab(crosstab_stacked(students, "university", var_map = var_maps))),
        capture.output(print(crosstab_stacked(students, "university", var_map = var_maps)))
    )
})

test_that("print.crosstab_data() works with grouped data", {
    expect_output(print.crosstab_data(crosstab_data(sports_by_age, "age")))
    expect_output(print.crosstab_data(crosstab_data(length_by_species, "species")))
    expect_output(print.crosstab_data(crosstab_data(licorice_by_region, "region", default_var_map(licorice_by_region$opinion))))
    expect_output(print.crosstab_data(crosstab_data(allergies_by_school, "school")))
})

test_that("print() calls print.crosstab_data() on grouped data", {
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(sports_by_age, "age"))),
        capture.output(print(crosstab_data(sports_by_age, "age")))
    )
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(length_by_species, "species"))),
        capture.output(print(crosstab_data(length_by_species, "species")))
    )
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(licorice_by_region, "region", default_var_map(licorice_by_region$opinion)))),
        capture.output(print(crosstab_data(licorice_by_region, "region", default_var_map(licorice_by_region$opinion))))
    )
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(allergies_by_school, "school"))),
        capture.output(print(crosstab_data(allergies_by_school, "school")))
    )
})

test_that("print.crosstab_data() works with ungrouped data", {
    expect_output(print.crosstab_data(crosstab_data(sports_by_age$sport)))
    expect_output(print.crosstab_data(crosstab_data(length_by_species$`petal length`)))
    expect_output(print.crosstab_data(crosstab_data(licorice_by_region$opinion, var_map = default_var_map(licorice_by_region$opinion))))
    expect_output(print.crosstab_data(crosstab_data(allergies_by_school$allergies)))
})

test_that("print() calls print.crosstab_data() with ungrouped data", {
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(sports_by_age$sport))),
        capture.output(print(crosstab_data(sports_by_age$sport)))
    )
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(length_by_species$`petal length`))),
        capture.output(print(crosstab_data(length_by_species$`petal length`)))
    )
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(licorice_by_region$opinion, var_map = default_var_map(licorice_by_region$opinion)))),
        capture.output(print(crosstab_data(licorice_by_region$opinion, var_map = default_var_map(licorice_by_region$opinion))))
    )
    expect_equal(
        capture.output(print.crosstab_data(crosstab_data(allergies_by_school$allergies))),
        capture.output(print(crosstab_data(allergies_by_school$allergies)))
    )
})
