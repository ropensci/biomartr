context("Test: getGenome()")

test_that("The getGenome() interface works properly for NCBI RefSeq..", {
    skip_on_cran()
    
    # test proper download from genbank
    Ath_Genome <- read_genome(
        getGenome(
            db       = "refseq",
            organism = "Arabidopsis thaliana",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    Ath_Genome <- read_genome(
        getGenome(
            db       = "refseq",
            organism = "Arabidopsis thaliana",
            path     = tempdir()
        ),
        format = "fasta"
    )
})

test_that("The getGenome() interface works properly for NCBI Genbank..", {
    skip_on_cran()
    
    # test proper download from genbank
    Ath_Genome <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "Arabidopsis thaliana",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    Ath_Genome <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "Arabidopsis thaliana",
            path     = tempdir()
        ),
        format = "fasta"
    )
})

test_that("The getGenome() interface works properly for ENSEMBL..",{
        
        skip_on_cran()
        
    # test proper download from ENSEMBL
    Scerevisiae_Genome <-
        read_genome(
            getGenome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
})

test_that("The getGenome() interface works properly for ENSEMBL..", {
    skip_on_cran()
    
    # test proper download from ENSEMBLGENOMES
    Ath_Genome <-
        read_genome(
            getGenome(
                db       = "ensemblgenomes",
                organism = "Arabidopsis thaliana",
                path     = tempdir()
            ),
            format = "fasta"
        )
})

test_that("The getGenome() interface works properly for ENSEMBL..", {
    skip_on_cran()
    
    # test proper use of internal referece files when command is repeated
    Ath_Genome <-
        read_genome(
            getGenome(
                db       = "ensemblgenomes",
                organism = "Arabidopsis thaliana",
                path     = tempdir()
            ),
            format = "fasta"
        )
})


test_that("The getGenome() error messages work properly for ENSEMBLGENOMES..",
          {
              skip_on_cran()
              expect_warning(
                  getGenome(
                      db       = "ensemblgenomes",
                      organism = "Saccharomyces cerevisi",
                      path     = tempdir()
                  )
              )
          })

test_that("The getGenome() error messages work properly for ENSEMBL..", {
    skip_on_cran()
    expect_warning(getGenome(
        db       = "ensembl",
        organism = "Saccharomyces cerevisi",
        path     = tempdir()
    ))
})

test_that("The getGenome() error messages work properly for NCBI RefSeq", {
    skip_on_cran()
    expect_message(getGenome(
        db       = "refseq",
        organism = "Saccharomycesi",
        path     = tempdir()
    ))
})












