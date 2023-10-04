context("Test: getBioSet()")

test_that("The getGFFSet() interface works properly for ensembl (repeating command)..",{
  skip_on_cran()
  skip_on_travis()
  path <- file.path(tempdir(), "GFF_set")
  out1 <- getGFFSet("ensembl",
                    organisms = c("Saccharomyces cerevisiae", "Escherichia coli"),
                    path = path, mute_citation = T)
  out2 <- getGFFSet("ensembl",
                    organisms = c("Saccharomyces cerevisiae", "Escherichia coli"),
                    path = path, mute_citation = T)
  expect_equal(length(out1), 2)
  expect_equal(as.character(out1), as.character(out2))
})
