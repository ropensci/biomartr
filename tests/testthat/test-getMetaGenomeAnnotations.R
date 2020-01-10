context("Test: getMetaGenomeAnnotations()")

test_that("The getMetaGenomeAnnotations() throws error when wrong 'name'
          argument is selected..",{
            skip_on_cran()
            skip_on_travis()
            
    expect_error(getMetaGenomeAnnotations(name = "somethingelse"))
})
