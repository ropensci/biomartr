context("Test: getAttributes()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getAttributes() interface works properly..",{
        
        skip_on_cran()
        
        marts <- getMarts()[ , "mart"]
        plant_mart <- which(stringr::str_match(marts, "plants_mart_") != "NA")
        
        attrib <- getAttributes(mart = as.character(marts[plant_mart]), dataset = "athaliana_eg_gene")
        
        expect_true(equal_lists(as.list(attrib[1 , ]), list(name = "ensembl_gene_id",
                                                            description = "Gene stable ID")))
})