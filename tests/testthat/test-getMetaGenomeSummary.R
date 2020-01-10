context("Test: getMetaGenomeSummary()")

test_that("The getMetaGenomeSummary() interface works properly..",{
    skip_on_cran()
    skip_on_travis()
    expect_output(getMetaGenomeSummary())
})
