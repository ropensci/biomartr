context("Test: is.genome.available()")

test_that("The is.genome.available() interface works properly..",{
        
        skip_on_cran()
        
        # test whether interface to 'refseq' works properly
        g <- is.genome.available(organism = "Arabidopsis thaliana", details = TRUE, db = "refseq")
        expect_identical(as.character(g[1, 1]),"Arabidopsis thaliana")
        
        # test with a second run using locally stored information
        is.genome.available(organism = "Homo sapiens", details = TRUE)
        
        # test with a second run using locally stored information without details
        is.genome.available(organism = "Homo sapiens")
        
        # test whether interface to 'genbank' works properly
        is.genome.available(organism = "Homo sapiens", details = TRUE, db = "genbank")
        
        # test whether it works properly a second time based on the internal files
        is.genome.available(organism = "Arabidopsis thaliana", details = TRUE, db = "genbank")
        
        # test whether it works properly a second time based on the internal files without details
        is.genome.available(organism = "Arabidopsis thaliana", db = "genbank")
        
        # test whether interface to 'ensembl' works properly
        is.genome.available(organism = "Homo sapiens", details = TRUE, db = "ensembl")
        
        # test whether interface to 'ensembl' works properly without details
        is.genome.available(organism = "Homo sapiens", db = "ensembl")
        
})


test_that("The is.genome.available() error messages work properly..",{
    
    # test unknown organism error handling for 'refseq'
    expect_error(is.genome.available(organism = "Homo sapi", db = "refseq"), "Unfortunately no entry for organism 'Homo sapi' could be found.")
    # test unknown organism error handling for 'genbank'
    expect_error(is.genome.available(organism = "Homo sapi", db = "genbank"), "Unfortunately no entry for organism 'Homo sapi' could be found.")
    # test unknown organism error handling for 'ensembl'
    expect_error(is.genome.available(organism = "Homo sapi", db = "ensembl"), "Unfortunately organism 'Homo sapi' is not available at ENSEMBL. Please check whether or not the organism name is typed correctly.")
    
    
})
