context("Test: getRNA()")

test_that("The getRNA() interface works properly..",{
    
    skip_on_cran()
    skip_on_travis() 
    
    # test proper download
    Ath_rna <- read_rna(getRNA( db       = "refseq",
                                organism = "Arabidopsis thaliana",
                                path     = tempdir()), format = "fasta")
    
    # test proper use of internal referece files when command is repeated
    Ath_rna <- read_rna(getRNA( db       = "refseq",
                                organism = "Arabidopsis thaliana",
                                path     = tempdir()), format = "fasta")
    
    # test proper download from genbank
    Ath_rna <- read_rna(getRNA( db       = "genbank",
                                organism = "Arabidopsis thaliana",
                                path     = tempdir()), format = "fasta")
    
    # test proper use of internal referece files when command is repeated
    Ath_rna <- read_rna(getRNA( db       = "genbank",
                                organism = "Arabidopsis thaliana",
                                path     = tempdir()), format = "fasta")
    
    # test proper download from ensemblgenomes
    Ath_rna <- read_rna(getRNA( db       = "ensemblgenomes",
                                organism = "Arabidopsis thaliana",
                                path     = tempdir()), format = "fasta")
    
})

test_that("The getRNA() error messages work properly..",{
    skip_on_cran()
    
    expect_error(getRNA( db       = "ensembl",
                        organism = "Saccharomyces cerevisi",
                        path     = tempdir()))
})
