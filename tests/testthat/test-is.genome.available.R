context("Test: is.genome.available()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The is.genome.available() interface works properly..",{
        
        g <- is.genome.available(organism = "Arabidopsis thaliana", details = TRUE)
        expect_identical(as.character(g[1, 1]),"Arabidopsis thaliana")
})