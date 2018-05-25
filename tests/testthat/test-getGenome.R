context("Test: getGenome()")

test_that("The getGenome() interface works properly for NCBI RefSeq (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    read_genome(
        getGenome(
            db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    read_genome(
        getGenome(
            db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir()
        ),
        format = "fasta"
    )
})


test_that("The getGenome() interface works properly for NCBI RefSeq using taxid..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    read_genome(
        getGenome(
            db       = "refseq",
            organism = "4932",
            path     = tempdir()
        ),
        format = "fasta"
    )
})

test_that("The getGenome() interface works properly for NCBI RefSeq using assembly id..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    read_genome(
        getGenome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
})



test_that("The getGenome() interface works properly for NCBI Genbank (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    read_genome(
        getGenome(
            db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    read_genome(
        getGenome(
            db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir()
        ),
        format = "fasta"
    )
})

test_that("The getGenome() interface works properly for ENSEMBL..",{
        
        skip_on_cran()
        skip_on_travis()    
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

test_that("The getGenome() interface works properly for ENSEMBLGENOMES..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from ENSEMBLGENOMES
        read_genome(
            getGenome(
                db       = "ensemblgenomes",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
})

test_that("The getGenome() interface works properly for ENSEMBLGENOMES..", {
    skip_on_cran()
    skip_on_travis()
    # test proper use of internal referece files when command is repeated
        read_genome(
            getGenome(
                db       = "ensemblgenomes",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
})


test_that("The getGenome() error messages work properly for ENSEMBLGENOMES..",
          {
              skip_on_cran()
              skip_on_travis()
                  getGenome(
                      db       = "ensemblgenomes",
                      organism = "Saccharomyces cerevisi",
                      path     = tempdir()
                  )
              
          })

test_that("The getGenome() error messages work properly for ENSEMBL..", {
    skip_on_cran()
    skip_on_travis()
    
    getGenome(
        db       = "ensembl",
        organism = "Saccharomyces cerevisi",
        path     = tempdir()
    )
})

test_that("The getGenome() error messages work properly for NCBI RefSeq", {
    skip_on_cran()
    skip_on_travis()
    
    expect_equal(getGenome(
        db       = "refseq",
        organism = "Saccharomycesi",
        path     = tempdir()
    ), "Not available")
})












