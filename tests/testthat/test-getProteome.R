context("Test: getProteome()")

test_that("The getProteome() interface works properly..",{

        skip_on_cran()

        # test proper download
        Ath_Proteome <- read_proteome(getProteome( db       = "refseq",
                                              organism = "Arabidopsis thaliana",
                                              path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_Proteome <- read_proteome(getProteome( db       = "refseq",
                                              organism = "Arabidopsis thaliana",
                                              path     = tempdir()), format = "fasta")
        
        # test proper download from genbank
        Ath_Proteome <- read_proteome(getProteome( db       = "genbank",
                                                   organism = "Arabidopsis thaliana",
                                                   path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_Proteome <- read_proteome(getProteome( db       = "genbank",
                                                   organism = "Arabidopsis thaliana",
                                                   path     = tempdir()), format = "fasta")
        
})

test_that("The getProteome() error messages work properly..",{
    skip_on_cran()
    expect_warning(getProteome( db       = "ensembl",
                        organism = "Saccharomyces cerevisi",
                        path     = tempdir()))
})
