context("Test: getGENOMEREPORT()")

test_that("The getGENOMEREPORT() interface works properly..",{
    skip_on_cran()
    skip_on_travis()
    # NCBI GENOMEREPORT Info file retrieval
    getGENOMEREPORT()
})
