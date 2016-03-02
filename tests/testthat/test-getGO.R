context("Test: getGO()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getGO() interface works properly..",{
        
        skip_on_cran()
        
        GO_tbl <- getGO(organism = "Homo sapiens",
                        genes    = "GUCA2A",
                        filters  = "hgnc_symbol")
        
        expect_true(identical(GO_tbl[1 , c("hgnc_symbol")], "GUCA2A"))
})





