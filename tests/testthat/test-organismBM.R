context("Test: organismBM()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}

test_that("The organismBM() interface works properly..",{
        
        skip_on_cran()
        skip_on_travis()
        
        oBM <- organismBM("Homo sapiens")
        expect_identical(as.character(oBM[1, 1]),"hsapiens")
})
