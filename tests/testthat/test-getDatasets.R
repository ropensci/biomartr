context("Test: getDatasets()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getDatasets() interface works properly..",{
        
        skip_on_cran()
        skip_on_travis()
        
    marts <- getMarts()[, "mart"]
    ex_mart <-
        which(stringr::str_match(marts, "ENSEMBL_MART_ENSEMBL") != "NA")
    
    attrib <-
        getDatasets(mart = as.character(marts$mart[ex_mart]))
    
})





