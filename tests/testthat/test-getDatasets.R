context("Test: getDatasets()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getDatasets() interface works properly..",{
        
        skip_on_cran()
        
        marts <- getMarts()[ , "mart"]
        plant_mart <- which(stringr::str_match(marts, "plants_mart_") != "NA")
        
        attrib <- getDatasets(mart = as.character(marts[plant_mart]))
        
        expect_true(equal_lists(as.list(attrib[1 , ]), list(dataset = "atauschii_eg_gene",
                                                            description = "Aegilops tauschii genes (ASM34733v1 (2013-12-BGI))",
                                                            version = "ASM34733v1 (2013-12-BGI)")))
})





