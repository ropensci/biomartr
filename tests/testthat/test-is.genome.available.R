context("Test: is.genome.available()")

test_that("The is.genome.available() interface works properly..",{
        
        skip_on_cran()
        
        # test whether interface to 'refseq' works properly
        g <- is.genome.available(organism = "Arabidopsis thaliana", details = TRUE, db = "refseq")
        expect_identical(as.character(g[1, 1]),"Arabidopsis thaliana")
        
        # test with a second run using locally stored information
        is.genome.available(organism = "Homo sapiens", details = TRUE)
        
        # test whether interface to 'genbank' works properly
        is.genome.available(organism = "Homo sapiens", details = TRUE, db = "genbank")
        
        # test whether it works properly a second time based on the internal files
        is.genome.available(organism = "Arabidopsis thaliana", details = TRUE, db = "genbank")
})
