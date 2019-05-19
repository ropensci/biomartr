context("Test: is.genome.available()")

test_that("The is.genome.available() interface to NCBI RefSeq 
          works properly..", {
    skip_on_cran()
    skip_on_travis()
    # test whether interface to 'refseq' works properly
        expect_equal(nrow(is.genome.available(db = "refseq",
                            organism = "Saccharomyces cerevisiae",
                            details = TRUE)) > 0, TRUE)

    # test whether it works properly a second time based on the internal files
        expect_equal(suppressMessages(is.genome.available(db = "refseq",
                        organism = "Saccharomyces cerevisiae",
                        details = FALSE)), TRUE)
    
})

test_that("The is.genome.available() interface to NCBI RefSeq 
          works properly using taxid..", {
              skip_on_cran()
              skip_on_travis()
              # test whether interface to 'refseq' works properly
              expect_equal(nrow(is.genome.available(db = "refseq",
                                  organism = "559292",
                                  details = TRUE)) > 0, TRUE)
              
              # test whether it works properly a second time based on the internal files
              expect_equal(suppressMessages(is.genome.available(db = "refseq",
                                  organism = "559292",
                                  details = FALSE)), TRUE)
              
          })


test_that("The is.genome.available() interface to NCBI RefSeq 
          works properly using assembly accession id", {
              skip_on_cran()
              skip_on_travis()
              # test whether interface to 'refseq' works properly
              expect_equal(nrow(is.genome.available(db = "refseq",
                                                    organism = "GCF_000146045.2",
                                                    details = TRUE)) > 0, TRUE)
              
              # test whether it works properly a second time based on the internal files
              expect_equal(suppressMessages(is.genome.available(db = "refseq",
                                                                organism = "GCF_000146045.2",
                                                                details = FALSE)), TRUE)
              
          })

test_that("The is.genome.available() interface to NCBI Genbank 
          works properly..", {
              skip_on_cran()
              skip_on_travis()  
    # test whether interface to 'genbank' works properly
              expect_equal(nrow(is.genome.available(db = "genbank",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)) > 0, TRUE)
    
    # test whether it works properly a second time based on the internal files
              expect_equal(suppressMessages(is.genome.available(db = "genbank",
                        organism = "Saccharomyces cerevisiae",
                        details = FALSE)), TRUE)
    
})

test_that("The is.genome.available() interface to NCBI Genbank 
          works properly using taxid..", {
              skip_on_cran()
              skip_on_travis()  
              # test whether interface to 'genbank' works properly
              expect_equal(nrow(is.genome.available(db = "genbank",
                                  organism = "559292",
                                  details = TRUE)) > 0, TRUE)
              
              # test whether it works properly a second time based on the internal files
              expect_equal(suppressMessages(is.genome.available(db = "genbank",
                                  organism = "559292",
                                  details = FALSE)), TRUE)
              
          })

test_that("The is.genome.available() interface to NCBI Genbank 
          works properly using assembly accession id", {
              skip_on_cran()
              skip_on_travis()  
              # test whether interface to 'genbank' works properly
              expect_equal(nrow(is.genome.available(db = "genbank",
                                                    organism = "GCA_000091065.2",
                                                    details = TRUE)) > 0, TRUE)
              
              # test whether it works properly a second time based on the internal files
              expect_equal(suppressMessages(is.genome.available(db = "genbank",
                                                                organism = "GCA_000091065.2",
                                                                details = FALSE)), TRUE)
              
          })

test_that("The is.genome.available() interface to Ensembl 
          works properly..", {
              skip_on_cran()
              skip_on_travis()
    # test whether interface to 'ensembl' works properly
              expect_equal(nrow(is.genome.available(db = "ensembl",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)) > 0, TRUE)
    
    # test whether interface to 'ensembl' works properly without details
              expect_equal(suppressMessages(is.genome.available(db = "ensembl", 
                            organism = "Saccharomyces cerevisiae")), TRUE)
})

test_that("The is.genome.available() interface to Ensembl 
          works properly using taxid..", {
              skip_on_cran()
              skip_on_travis()
              # test whether interface to 'ensembl' works properly
              expect_equal(nrow(is.genome.available(db = "ensembl",
                                                    organism = "4932",
                                                    details = TRUE)) > 0, TRUE)
              
              # test whether interface to 'ensembl' works properly without details
              expect_equal(suppressMessages(is.genome.available(db = "ensembl", 
                                                                organism = "4932")), TRUE)
          })


test_that("The is.genome.available() interface to Ensembl 
          works properly using accession id", {
              skip_on_cran()
              skip_on_travis()
              # test whether interface to 'ensembl' works properly
              expect_equal(nrow(is.genome.available(db = "ensembl",
                                                    organism = "GCA_000146045.2",
                                                    details = TRUE)) > 0, TRUE)
              
              # test whether interface to 'ensembl' works properly without details
              expect_equal(suppressMessages(is.genome.available(db = "ensembl", 
                                                                organism = "GCA_000146045.2")), TRUE)
          })

test_that("The is.genome.available() error messages work properly..", {
    skip_on_cran()
        skip_on_travis()
    # test unknown organism error handling for 'refseq'
    expect_identical(suppressMessages(is.genome.available(organism = "Hrmo sapi", 
                                         db = "refseq")),
                     FALSE)
    # test unknown organism error handling for 'genbank'
    expect_identical(suppressMessages(is.genome.available(organism = "Hrmo sapi", 
                                         db = "genbank")),
                     FALSE)
    # test unknown organism error handling for 'ensembl'
    expect_identical(suppressMessages(is.genome.available(organism = "Hrmo sapi", db = "ensembl")),
                     FALSE)
    
    expect_identical(suppressMessages(
        is.genome.available(organism = "Hrmo sapi", db = "ensemblgenomes")
    ),
    FALSE)
    
    expect_identical(suppressMessages(is.genome.available(organism = "Hrmo sapi", db = "uniprot")),
                     FALSE)
})


test_that("The is.genome.available() interface to Uniprot 
          works properly..", {
              skip_on_cran()
              skip_on_travis()
              # test whether interface to 'uniprot' works properly
              is.genome.available(db = "uniprot",
                                  organism = "Saccharomyces cerevisiae",
                                  details = TRUE)
              
              # test whether interface to 'uniprot' works properly without details
              is.genome.available(db = "uniprot", 
                                  organism = "Saccharomyces cerevisiae")
              
          })




