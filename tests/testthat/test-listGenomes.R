context("Test: listGenomes()")

test_that("The listGenomes() interface works properly for NCBI RefSeq (repeating command)..",{

  skip_on_cran()
  skip_on_travis()
  # test proper download from refseq
  out1 <- listGenomes(db = "refseq", type = "kingdom", subset = "Eukaryota")
  out2 <- listGenomes(db = "refseq", type = "kingdom", subset = "Eukaryota")
  out3 <- listGenomes(db = "refseq", type = "kingdom", subset = "Viruses")
  expect_equal(out1, out2)
  expect_false(isTRUE(all.equal(out2, out3)))
})

test_that("The listGenomes() interface works properly for Ensembl (repeating command)..",{

  skip_on_cran()
  skip_on_travis()
  # test proper download from refseq
  out1 <- listGenomes(db = "ensembl", type = "kingdom", subset = "EnsemblFungi")
  out2 <- listGenomes(db = "ensembl", type = "kingdom", subset = "EnsemblFungi")
  out3 <- listGenomes(db = "ensembl", type = "kingdom", subset = "EnsemblVertebrates")
  expect_equal(out1, out2)
  expect_false(isTRUE(all.equal(out2, out3)))
})

test_that("The listGenomes() fails properly",{

  skip_on_cran()
  skip_on_travis()
  # refseq fails
  expect_error(listGenomes(db = "refseq", type = "kingdom", subset = "Eukaryota1"))
  # ensembl fails
  expect_error(listGenomes(db = "ensembl", type = "kingdom", subset = "Eukaryota1"))
})
