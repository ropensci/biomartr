context("Test: download.database()")


test_that("The download.database() throws error when wrong input database is specified..",{
    
    skip_on_cran()
    skip_on_travis()
    
    expect_error(download.database(db = "nl"), "The specified database 'nl' could not be found on NCBI. Please use the listDatabases() command to retrieve available databases or check if the name was written correctly.")
    
})
