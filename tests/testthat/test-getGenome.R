context("Test: getGenome()")

test_that("The getGenome() interface works properly..",{

        skip_on_cran()
        skip_on_travis() 
        
        # test proper download
        Ath_Genome <- read_genome(getGenome( db       = "refseq",
                                          organism = "Arabidopsis thaliana",
                                          path     = tempdir()), format = "fasta")
        
        # test proper use of internal referece files when command is repeated
        Ath_Genome <- read_genome(getGenome( db       = "refseq",
                                          organism = "Arabidopsis thaliana",
                                          path     = tempdir()), format = "fasta")
        
})
