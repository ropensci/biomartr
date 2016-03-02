context("Test: getGenome()")

test_that("The getGenome() interface works properly..",{

        skip_on_cran()
        skip_on_travis() 
        
          getGenome( db       = "refseq",
                     kingdom  = "plant",
                     organism = "Arabidopsis thaliana",
                     path     = tempdir())
        
        
        file_path <- file.path(tempdir(),"Arabidopsis_thaliana_genomic.fna.gz")
        Ath_Genome <- read_cds(file_path, format = "fasta")
        
        expect_identical(Ath_Genome[1,geneids],"NC_000932.1")
})
