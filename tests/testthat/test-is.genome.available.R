context("Test: is.genome.available()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The is.genome.available() interface works properly..",{
        
        skip_on_cran()
        
        g <- is.genome.available(organism = "Arabidopsis thaliana", details = TRUE)
        expect_identical(as.character(g[1, 1]),"Arabidopsis thaliana")
        
        # test with a second run using locally stored information
        is.genome.available(organism = "Homo sapiens", details = TRUE)
        
})
