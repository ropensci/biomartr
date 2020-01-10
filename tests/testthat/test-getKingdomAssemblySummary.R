context("Test: getKingdomAssemblySummary()")

test_that("The getKingdomAssemblySummary() interface works properly
          for NCBI RefSeq..",{
    skip_on_cran()
    skip_on_travis()             
    # KingdomAssembly Summary Info file retrieval from RefSeq
    expect_output(getKingdomAssemblySummary("refseq"))
})


test_that("The getKingdomAssemblySummary() interface works properly
          for NCBI Genbank..",{
              skip_on_cran()
              skip_on_travis()          
              # KingdomAssembly Summary Info file retrieval from Genbank
              expect_output(getKingdomAssemblySummary("genbank"))
})
