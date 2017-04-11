context("Test: getMetaGenomeAnnotations()")

test_that("The getMetaGenomeAnnotations() throws error when wrong 'name'
          argument is selected..",{
    
    expect_error(getMetaGenomeAnnotations(name = "somethingelse"))
})
