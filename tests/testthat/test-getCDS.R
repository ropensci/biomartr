context("Test: getCDS()")

test_that("The getCDS() interface works properly..",{
       
        skip_on_cran()
        skip_on_travis() 
        
        # test proper download
        Ath_CDS <- read_cds(getCDS( db       = "refseq",
                                    organism = "Arabidopsis thaliana",
                                    path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_CDS <- read_cds(getCDS( db       = "refseq",
                                    organism = "Arabidopsis thaliana",
                                    path     = tempdir()), format = "fasta")
        
        # test proper download from genbank
        Ath_CDS <- read_cds(getCDS( db       = "genbank",
                                    organism = "Arabidopsis thaliana",
                                    path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_CDS <- read_cds(getCDS( db       = "genbank",
                                    organism = "Arabidopsis thaliana",
                                    path     = tempdir()), format = "fasta")
        
})

test_that("The getCDS() error messages work properly..",{
    
    expect_true(getCDS( db       = "ensembl",
                        organism = "Saccharomyces cerevisi",
                        path     = tempdir()))
})
