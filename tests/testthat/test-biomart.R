context("Test: biomart()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The biomart() interface works properly..",{

        skip_on_cran()
        skip_on_travis()
        
    marts <- getMarts()[, "mart"]
    ex_mart <-
        which(stringr::str_match(marts, "ENSEMBL_MART_ENSEMBL") != "NA")
    
    expect_success(
            biomart(
                genes      = "GUCA2A",
                mart       = as.character(marts$mart[ex_mart]),
                dataset    = "hsapiens_gene_ensembl",
                attributes = c("start_position", "end_position", "description"),
                filters    = "hgnc_symbol"
            )[1, 1:3]
        )

})
