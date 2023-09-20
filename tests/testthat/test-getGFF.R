context("Test: getGFF()")

test_that("The getGFF() interface works properly for NCBI RefSeq (repeating command)..",{

        skip_on_cran()
        skip_on_travis()
    # test proper download from refseq
    getGFF( db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())

    getGFF( db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())
})

test_that("The getGFF() interface works properly for NCBI RefSeq using taxid (repeating command)..",{

    skip_on_cran()
    skip_on_travis()
    # test proper download from refseq
    getGFF( db       = "refseq",
            organism = "559292",
            path = tempdir())

    getGFF( db       = "refseq",
            organism = "559292",
            path = tempdir())
})

test_that("The getGFF() interface works properly for NCBI Genbank (repeating command)..",{
        skip_on_travis()
        skip_on_cran()
    # test proper download from genbank
    getGFF( db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())

    getGFF( db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())
})


test_that("The getGFF() interface works properly for Ensembl (repeating command)",{
        skip_on_cran()
        skip_on_travis()

    # test proper download from Ensembl
    getGFF( db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())

    getGFF( db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())
})

test_that("The getGTF() interface works properly for Ensembl (repeating command)",{
  skip_on_cran()
  skip_on_travis()

  # test proper download from Ensembl
  getGTF( db       = "ensembl",
          organism = "Saccharomyces cerevisiae",
          path = tempdir())

  getGTF( db       = "ensembl",
          organism = "Saccharomyces cerevisiae",
          path = tempdir())
})

test_that("The getGFF() interface works properly for EnsemblGenomes with collections",{
  skip_on_cran()
  skip_on_travis()

  # Bacteria
  getGFF( db       = "ensembl",
          organism = "Escherichia coli",
          path = tempdir(), mute_citation = TRUE)
  # Fungi
  getGFF( db       = "ensembl",
          organism = "Acremonium chrysogenum",
          path = tempdir(), mute_citation = TRUE)
  # Protists
  getGFF( db       = "ensembl",
          organism = "Babesia bigemina",
          path = tempdir(), mute_citation = TRUE)
})

test_that("The getGTF() interface works properly for EnsemblGenomes with collections",{
  skip_on_cran()
  skip_on_travis()

  # Bacteria
  getGTF( db       = "ensembl",
          organism = "Escherichia coli",
          path = tempdir(), mute_citation = TRUE)
  # Fungi
  getGTF( db       = "ensembl",
          organism = "Acremonium chrysogenum",
          path = tempdir(), mute_citation = TRUE)
  # Protists
  getGTF( db       = "ensembl",
          organism = "Babesia bigemina",
          path = tempdir(), mute_citation = TRUE)
})


