context("Test: getFilters()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getFilters() interface works properly..",{
        
        skip_on_cran()
        
    marts <- getMarts()$mart
    elsembl_mart <-
        which(stringr::str_match(marts, "ENSEMBL_MART_ENSEMBL") != "NA")
    
    attrib <-
        getFilters(mart = as.character(marts[elsembl_mart]), 
                   dataset = "hsapiens_gene_ensembl")
    
    expect_equal(attrib[1 , 1], "chromosome_name")
        
})





