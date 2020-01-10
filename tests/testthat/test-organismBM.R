context("Test: organismBM()")

test_that("The organismBM() interface works properly..",{
        
        skip_on_cran()
        skip_on_travis()
        
        expect_output(organismBM("Homo sapiens"))
})
