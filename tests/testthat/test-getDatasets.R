context("Test: getDatasets()")

test_that("The getDatasets() interface works properly..",{

    skip_on_cran()
    skip_on_travis()

    expect_identical(getMarts()[1 , "mart"][[1]], "ENSEMBL_MART_ENSEMBL")
    expect_output(getDatasets(mart = "ENSEMBL_MART_ENSEMBL"))

})





