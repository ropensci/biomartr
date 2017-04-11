context("Test: download.database()")


test_that("The download.database() interface works properly..",{
    
    skip_on_cran()
    skip_on_travis()

    # download example NCBI nr file          
    # download.database(db = "nr.00.tar.gz", path = getwd())          
    # test if file was successfully downloaded          
    # expect_true(file.exists(file.path(getwd(), "nr.00.tar.gz")))
    
})


test_that("The download.database() throws error when wrong input
          database is specified..",{

     expect_error(download.database(db = "nr"))     
              
})
