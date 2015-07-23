context("Test: getFilters()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getFilters() interface works properly..",{
        
        skip_on_cran()
        
        marts <- getMarts()[ , "mart"]
        plant_mart <- which(stringr::str_match(marts, "plants_mart_") != "NA")
        
        attrib <- getFilters(mart = as.character(marts[plant_mart]), dataset = "athaliana_eg_gene")
        
        expect_true(equal_lists(as.list(attrib[1 , ]), list(name = "chromosome_name",
                                                            description = "Chromosome name")))
})





