context("Test: getGFF()")

test_that("The getGFF() interface works properly for NCBI RefSeq (repeating command)..",{

        skip_on_cran()
        skip_on_travis()
    # test proper download from refseq
    out1 <- getGFF( db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path = tempdir(), mute_citation = TRUE)

    out2 <- getGFF( db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path = tempdir(), mute_citation = TRUE)
    expect_equal(out1, out2)
})

test_that("The getGFF() interface works properly for NCBI RefSeq using taxid (repeating command)..",{

    skip_on_cran()
    skip_on_travis()
    # test proper download from refseq
    out1 <- getGFF( db       = "refseq",
            organism = "559292",
            path = tempdir(), mute_citation = TRUE)

    out2 <- getGFF( db       = "refseq",
            organism = "559292",
            path = tempdir(), mute_citation = TRUE)
    expect_equal(out1, out2)
})

test_that("The getGFF() interface works properly for NCBI Genbank (repeating command)..",{
        skip_on_travis()
        skip_on_cran()
    # test proper download from genbank
    out1 <- getGFF( db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path = tempdir(), mute_citation = TRUE)

    out2 <- getGFF( db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path = tempdir(), mute_citation = TRUE)
    expect_equal(out1, out2)
})

test_that("The getGFF() interface works properly for NCBI RefSeq (GTF format)",{

  skip_on_cran()
  skip_on_travis()
  # test proper download from refseq
  out1 <- getGFF( db       = "refseq",
                  organism = "Saccharomyces cerevisiae",
                  path = tempdir(), mute_citation = TRUE, format = "gtf")

  out2 <- getGFF( db       = "refseq",
                  organism = "Saccharomyces cerevisiae",
                  path = tempdir(), mute_citation = TRUE, format = "gtf")
  expect_equal(out1, out2)
})



test_that("The getGFF() interface works properly for Ensembl (repeating command)",{
        skip_on_cran()
        skip_on_travis()

    # test proper download from Ensembl
    out1 <- getGFF( db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path = tempdir(), mute_citation = TRUE)

    out2 <- getGFF( db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path = tempdir(), mute_citation = TRUE)
    expect_equal(out1, out2)
})

test_that("The getGTF() interface works properly for Ensembl (repeating command)",{
  skip_on_cran()
  skip_on_travis()

  # test proper download from Ensembl
  out1 <- getGTF( db       = "ensembl",
          organism = "Saccharomyces cerevisiae",
          path = tempdir(), mute_citation = TRUE)

  out2 <- getGTF( db       = "ensembl",
          organism = "Saccharomyces cerevisiae",
          path = tempdir(), mute_citation = TRUE)
  expect_equal(out1, out2)
})

test_that("The getGFF() interface works properly for EnsemblGenomes with collections",{
  skip_on_cran()
  skip_on_travis()

  # Bacteria
  out1 <- getGFF( db       = "ensembl",
          organism = "Escherichia coli",
          path = tempdir(), mute_citation = TRUE)
  # Fungi
  out2 <- getGFF( db       = "ensembl",
          organism = "Acremonium chrysogenum",
          path = tempdir(), mute_citation = TRUE)
  # Protists
  getGFF( db       = "ensembl",
          organism = "Babesia bigemina",
          path = tempdir(), mute_citation = TRUE)
  expect_false(out1 == out2)
})

test_that("The getGTF() interface works properly for EnsemblGenomes with collections",{
  skip_on_cran()
  skip_on_travis()

  # Bacteria
  out1 <- getGTF( db       = "ensembl",
          organism = "Escherichia coli",
          path = tempdir(), mute_citation = TRUE)
  # Fungi
  out2 <- getGTF( db       = "ensembl",
          organism = "Acremonium chrysogenum",
          path = tempdir(), mute_citation = TRUE)
  # Protists
  getGTF( db       = "ensembl",
          organism = "Babesia bigemina",
          path = tempdir(), mute_citation = TRUE)
  expect_false(out1 == out2)
})


