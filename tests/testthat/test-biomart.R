context("Test: biomart()")

test_that("The biomart() interface works properly..",{

    skip_on_cran()
    skip_on_travis()

    expect_gt(nrow(getMarts(update = TRUE)), 6)
    expect_output(
            biomart(
                genes      = "GUCA2A",
                mart       = "ENSEMBL_MART_ENSEMBL",
                dataset    = "hsapiens_gene_ensembl",
                attributes = c("start_position", "end_position", "description"),
                filters    = "hgnc_symbol"
            )[1, 1:3]
        )
})
