context("Test: getProteome()")

test_that("The getProteome() interface to NCBI RefSeq works properly..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    Proteome <-
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
    # test proper use of internal referece files when command is repeated
    Proteome <-
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
})

test_that("The getProteome() interface to NCBI Genbank works properly..", {
    skip_on_cran()
    skip_on_travis() 
    # test proper download from genbank
    Proteome <-
        read_proteome(
            getProteome(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
    # test proper use of internal referece files when command is repeated
    Proteome <-
        read_proteome(
            getProteome(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
})


test_that("The getProteome() interface to Ensembl works properly..", {
    skip_on_cran()
    skip_on_travis()
    Proteome <-
        read_proteome(
            getProteome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
})

test_that("The getProteome() interface to EnsemblGenomes works properly..", {
    skip_on_cran()
    skip_on_travis()    
    Proteome <-
        read_proteome(
            getProteome(
                db       = "ensemblgenomes",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
})

test_that("The getProteome() error messages work properly..", {
    skip_on_cran()
    skip_on_travis()     
    expect_warning(getProteome(
        db       = "ensembl",
        organism = "Saccharomyces cerevisi",
        path     = tempdir()
    ))
})
