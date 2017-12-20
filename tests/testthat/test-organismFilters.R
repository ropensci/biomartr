context("Test: organismFilters()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}

test_that("The organismFilters() interface works properly..",{
        
        skip_on_cran()
        skip_on_travis()
        
        oF <- organismFilters("Homo sapiens")
        expect_identical(as.character(oF[1, 1]),"chromosome_name")
})
