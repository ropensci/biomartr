context("Test: getGenome()")

test_that("The getGenome() interface works properly..",{

        skip_on_cran()
        skip_on_travis() 
        
        # test proper download from genbank
        Ath_Genome <- read_genome(getGenome( db       = "refseq",
                                          organism = "Arabidopsis thaliana",
                                          path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_Genome <- read_genome(getGenome( db       = "refseq",
                                          organism = "Arabidopsis thaliana",
                                          path     = tempdir()), format = "fasta")
        
        # test proper download from genbank
        Ath_Genome <- read_genome(getGenome( db       = "genbank",
                                             organism = "Arabidopsis thaliana",
                                             path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_Genome <- read_genome(getGenome( db       = "genbank",
                                             organism = "Arabidopsis thaliana",
                                             path     = tempdir()), format = "fasta")
        
        # test proper download from ENSEMBL
        Scerevisiae_Genome <- read_genome(getGenome( db       = "ensembl",
                                                     organism = "Saccharomyces cerevisiae",
                                                     path     = tempdir()), format = "fasta")
        
        # test proper download from ENSEMBLGENOMES
        Ath_Genome <- read_genome(getGenome( db       = "ensemblgenomes",
                                             organism = "Arabidopsis thaliana",
                                             path     = tempdir()), format = "fasta")
     
        # test proper use of internal referece files when command is repeated
        Ath_Genome <- read_genome(getGenome( db       = "ensemblgenomes",
                                             organism = "Arabidopsis thaliana",
                                             path     = tempdir()), format = "fasta")
        
})


test_that("The getGenome() error messages work properly..",{
    
    expect_error(getGenome( db       = "ensembl",
               organism = "Saccharomyces cerevisi",
               path     = tempdir()), "Unfortunately organism 'Saccharomyces cerevisi' is not available at ENSEMBL. Please check whether or not the organism name is typed correctly.")
})
