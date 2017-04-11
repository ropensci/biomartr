context("Test: getKingdomAssemblySummary()")

test_that("The getKingdomAssemblySummary() interface works properly
          for NCBI RefSeq..",{
    skip_on_cran()
    # KingdomAssembly Summary Info file retrieval from RefSeq
    getKingdomAssemblySummary("refseq")
})


test_that("The getKingdomAssemblySummary() interface works properly
          for NCBI Genbank..",{
              skip_on_cran()
              # KingdomAssembly Summary Info file retrieval from Genbank
              getKingdomAssemblySummary("genbank")
})
