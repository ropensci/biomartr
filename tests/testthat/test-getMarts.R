context("Test: getMarts()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getMarts() interface works properly..",{
        
        m <- getMarts()
        expect_identical(as.character(getMarts()[1, 1]),"ensembl")
})


