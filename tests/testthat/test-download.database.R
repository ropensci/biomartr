context("Test: download.database()")


test_that("The download.database() throws error when wrong input database is specified..",{
    
    skip_on_cran()

    expect_output(download.database(db = "nr.00.tar.gz"))
    
})
