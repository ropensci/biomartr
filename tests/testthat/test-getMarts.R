context("Test: getMarts()")

test_that("The getMarts() interface works properly..",{

        skip_on_cran()
        skip_on_travis()

        marts <- getMarts()
        expect_false(nrow(marts) == 0)
        expect_false(length(marts$mart) == length(unique(marts$mart)))
})


