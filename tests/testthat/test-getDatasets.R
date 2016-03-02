context("Test: getDatasets()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getDatasets() interface works properly..",{
        
        skip_on_cran()
        
        marts <- getMarts()[ , "mart"]
        ex_mart <- which(stringr::str_match(marts, "ENSEMBL_MART_ENSEMBL") != "NA")
        
        attrib <- getDatasets(mart = as.character(marts[ex_mart]))
        
        expect_true(equal_lists(as.list(attrib[1 , ]), list(dataset = "oanatinus_gene_ensembl",
                                                            description = "Ornithorhynchus anatinus genes (OANA5)",
                                                            version = "OANA5")))
})





