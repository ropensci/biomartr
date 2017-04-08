context("Test: organismAttributes()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}

test_that("The organismAttributes() interface works properly..",{
        
        skip_on_cran()
        
        oattr <- organismAttributes("Homo sapiens")
        expect_identical(as.character(oattr[1, 1]),"ensembl_gene_id")
})
