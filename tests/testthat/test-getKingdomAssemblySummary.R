context("Test: getKingdomAssemblySummary()")

test_that("The getKingdomAssemblySummary() interface works properly
          for NCBI RefSeq..",{
    skip_on_cran()
    skip_on_travis()
    # KingdomAssembly Summary Info file retrieval from RefSeq
    all <- getKingdomAssemblySummary("refseq")
    expect_false(nrow(all) == 0)
})


test_that("The getKingdomAssemblySummary() interface works properly
          for NCBI Genbank..",{
    skip_on_cran()
    skip_on_travis()
    # KingdomAssembly Summary Info file retrieval from Genbank
    all <- getKingdomAssemblySummary("genbank")
    expect_false(nrow(all) == 0)
})
