context("Test: organismBM()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}

test_that("The organismBM() interface works properly..",{
        
        oBM <- organismBM("Arabidopsis thaliana")
        expect_identical(as.character(oBM[1, 4]),"athaliana_eg_gene")
})