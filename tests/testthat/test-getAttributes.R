context("Test: getAttributes()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getAttributes() interface works properly..",{

        skip_on_cran()
        skip_on_travis()

    expect_equal(getMarts()$mart[1], "ENSEMBL_MART_ENSEMBL")

    attrib <-
        getAttributes(mart = "ENSEMBL_MART_ENSEMBL",
                      dataset = "hsapiens_gene_ensembl",
                      mute_citation = TRUE)

    expect_equal(attrib[1 , 1], "ensembl_gene_id")
})

test_that("The getAttributes() throws error when wrong mart is selected",{

    skip_on_cran()
    skip_on_travis()

    expect_error(getAttributes(mart = "somethingelse",
                  dataset = "hsapiens_gene_ensembl"))

})

test_that("The getAttributes() throws error when wrong dataset is selected",{

    skip_on_cran()
    skip_on_travis()

    expect_error(getAttributes(mart = "hsapiens_gene_ensembl",
                  dataset = "somethingelse"))

})




