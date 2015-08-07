context("Test: getGO()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getGO() interface works properly..",{
        
        skip_on_cran()
        
        GO_tbl <- getGO(organism = "Arabidopsis thaliana",
                        genes    = c("AT1G06090", "AT1G06100",
                                     "AT1G06110", "AT1G06120",
                                     "AT1G06130", "AT1G06200"),
                        filters  = "tair_locus")
        
        
        expect_true(equal_lists(as.list(GO_tbl[1 , c("tair_locus")]), list(tair_locus = "AT1G06090")))
})





