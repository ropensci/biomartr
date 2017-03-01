context("Test: getAttributes()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getAttributes() interface works properly..",{
        
        skip_on_cran()
        
        marts <- getMarts()[ , "mart"]
        ensemb_mart <- which(stringr::str_match(marts, "ENSEMBL_MART_ENSEMBL") != "NA")
        
        attrib <- getAttributes(mart = as.character(marts$mart[ensemb_mart]), dataset = "hsapiens_gene_ensembl")
        
        expect_true(equal_lists(as.list(attrib[1 , ]), list(name = "ensembl_gene_id",
                                                            description = "Gene ID")))
})
