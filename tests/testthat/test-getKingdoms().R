context("Test: getKingdoms()")

test_that("The getKingdoms() interface works properly..",{
    
    skip_on_cran()
    skip_on_travis() 
    testthat::equals(getKingdoms(), c("archaea", "bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", "vertebrate_other", "viral"))

})

