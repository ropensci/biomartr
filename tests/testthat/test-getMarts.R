context("Test: getMarts()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getMarts() interface works properly..",{
        
        skip_on_cran()
        skip_on_travis()
        
        m <- getMarts()
        expect_identical(as.character(getMarts()[1, 1]),"ENSEMBL_MART_ENSEMBL")
})


