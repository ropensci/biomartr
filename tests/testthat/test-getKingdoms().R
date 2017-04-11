context("Test: getKingdoms()")

test_that("The getKingdoms() interface works properly..",{
    
    testthat::equals(
        getKingdoms(),
        c(
            "archaea",
            "bacteria",
            "fungi",
            "invertebrate",
            "plant",
            "protozoa",
            "vertebrate_mammalian",
            "vertebrate_other",
            "viral"
        )
    )
    
})

