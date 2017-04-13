context("Test: is.genome.available()")

test_that("The is.genome.available() interface to NCBI RefSeq 
          works properly..", {
    skip_on_cran()
    
    # test whether interface to 'refseq' works properly
    g <-
        is.genome.available(db = "refseq",
                            organism = "Saccharomyces cerevisiae",
                            details = TRUE)

    # test whether it works properly a second time based on the internal files
    is.genome.available(db = "refseq",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)
    
})

test_that("The is.genome.available() interface to NCBI Genbank 
          works properly..", {
              skip_on_cran()
        
    # test whether interface to 'genbank' works properly
    is.genome.available(db = "genbank",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)
    
    # test whether it works properly a second time based on the internal files
    is.genome.available(db = "genbank",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)
    
})

test_that("The is.genome.available() interface to Ensembl 
          works properly..", {
              skip_on_cran()
    # test whether interface to 'ensembl' works properly
    is.genome.available(db = "ensembl",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)
    
    # test whether interface to 'ensembl' works properly without details
    is.genome.available(db = "ensembl", organism = "Saccharomyces cerevisiae")
})

test_that("The is.genome.available() interface to EnsemblGenomes 
          works properly..", {
              skip_on_cran()
    # test whether interface to 'ensemblgenomes' works properly
    is.genome.available(db = "ensemblgenomes",
                        organism = "Saccharomyces cerevisiae",
                        details = TRUE)
    
    # test whether interface to 'ensemblgenomes' works properly without details
    is.genome.available(db = "ensemblgenomes", 
                        organism = "Saccharomyces cerevisiae")
    
})


test_that("The is.genome.available() error messages work properly..", {
    skip_on_cran()
    
    # test unknown organism error handling for 'refseq'
    expect_identical(is.genome.available(organism = "Hrmo sapi", 
                                         db = "refseq"),
                     FALSE)
    # test unknown organism error handling for 'genbank'
    expect_identical(is.genome.available(organism = "Hrmo sapi", 
                                         db = "genbank"),
                     FALSE)
    # test unknown organism error handling for 'ensembl'
    expect_error(
        is.genome.available(organism = "Hrmo sapi", db = "ensembl")
    )
})
