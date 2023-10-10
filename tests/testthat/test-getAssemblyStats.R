context("Test: getAssemblyStats()")


test_that("The getAssemblyStats() downloads assembly stats file and reads
          raw input: NCBI RefSeq ..",{

    skip_on_cran()
    skip_on_travis()

    library(magrittr)

    Scerevisiae.stats <- getAssemblyStats(
        db = "refseq",
        organism = "Saccharomyces cerevisiae",
        path = tempdir(),
        mute_citation = TRUE
    )


    raw <- Scerevisiae.stats %>%
        read_assemblystats(type = "raw")

    stats <- Scerevisiae.stats %>%
        read_assemblystats(type = "stats")

    expect_false(nrow(raw) == nrow(stats))
})


test_that("The getAssemblyStats() downloads assembly stats file and reads raw
          input: NCBI Genbank ..",{

    skip_on_cran()
    skip_on_travis()

    Scerevisiae.stats <- getAssemblyStats(
        db = "genbank",
        organism = "Saccharomyces cerevisiae",
        path = tempdir(),
        mute_citation = TRUE
    )

    raw <- read_assemblystats(Scerevisiae.stats, type = "raw")

    stats <- read_assemblystats(Scerevisiae.stats, type = "stats")
    expect_false(nrow(raw) == nrow(stats))
})


test_that("The getAssemblyStats() downloads assembly stats file and imports file
          ..",{

    skip_on_cran()
    skip_on_travis()
    Scerevisiae.stats <- getAssemblyStats(
        db = "refseq",
        organism = "Saccharomyces cerevisiae",
        type = "import",
        path = tempdir(),
        mute_citation = TRUE
    )
    expect_false(nrow(Scerevisiae.stats) == 0)

})

test_that("The getAssemblyStats() throws error when undefined 'type' is selected
          ..",{

  skip_on_cran()
  skip_on_travis()
  expect_error(getAssemblyStats(
      db = "refseq",
      organism = "Saccharomyces cerevisiae",
      type = "somethingelse",
      path = tempdir(),
      mute_citation = TRUE))
})


test_that("The getAssemblyStats() throws error when undefined 'db' is selected
          ..",{

  skip_on_cran()
  skip_on_travis()
  expect_error(getAssemblyStats(
      db = "somethingelse",
      organism = "Saccharomyces cerevisiae",
      type = "download",
      path = tempdir(),
      mute_citation = TRUE))
})


test_that("The getAssemblyStats() throws error when undefined 'organism' is selected
          ..",{
  skip_on_cran()
  skip_on_travis()
  expect_message(getAssemblyStats(
      db = "refseq",
      organism = "somethingelse",
      type = "download",
      path = tempdir(),
      mute_citation = TRUE))
})




