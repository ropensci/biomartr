context("Test: getRNA()")

test_that("The getRNA() interface to NCBI RefSeq works properly..",{
    
    skip_on_cran()
    skip_on_travis()
    # test proper download
    RNA <- read_rna(getRNA( db       = "refseq",
                                organism = "Saccharomyces cerevisiae",
                                path     = tempdir()), format = "fasta")
    
    # test proper use of internal referece files when command is repeated
    RNA <- read_rna(getRNA( db       = "refseq",
                                organism = "Saccharomyces cerevisiae",
                                path     = tempdir()), format = "fasta")
})

test_that("The getRNA() interface to NCBI Genbank works properly..",{
    
    skip_on_cran()
    skip_on_travis()    
    # test proper download from genbank
    RNA <- read_rna(getRNA( db       = "genbank",
                                organism = "Saccharomyces cerevisiae",
                                path     = tempdir()), format = "fasta")
    
    # test proper use of internal referece files when command is repeated
    RNA <- read_rna(getRNA( db       = "genbank",
                                organism = "Saccharomyces cerevisiae",
                                path     = tempdir()), format = "fasta")

})


test_that("The getRNA() interface to Ensembl works properly..",{
    
    skip_on_cran()
    skip_on_travis()    
    # test proper download from ensemblgenomes
    RNA <- read_rna(getRNA( db       = "ensembl",
                            organism = "Saccharomyces cerevisiae",
                            path     = tempdir()), format = "fasta")
})


test_that("The getRNA() interface to EnsemblGenomes works properly..",{
    
    skip_on_cran()
    skip_on_travis()
# test proper download from ensemblgenomes
RNA <- read_rna(getRNA( db       = "ensemblgenomes",
                        organism = "Saccharomyces cerevisiae",
                        path     = tempdir()), format = "fasta")
})


test_that("The getRNA() error messages work properly..",{
    skip_on_cran()
    skip_on_travis()
    expect_warning(getRNA( db       = "ensembl",
                        organism = "Saccharomyces cerevisi",
                        path     = tempdir()))
})
