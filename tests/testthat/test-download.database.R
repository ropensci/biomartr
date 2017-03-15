context("Test: download.database()")


test_that("The download.database() throws error when wrong input database is specified..",{
    
    skip_on_cran()
    skip_on_travis()
    
    expect_error(download.database(db = "nl"))
    
})
