context("Test: is.genome.available()")

test_that("The is.genome.available() interface works properly..",{
        
        skip_on_cran()
        
        # test whether interface to 'refseq' works properly
        g <- is.genome.available(db = "refseq", organism = "Arabidopsis thaliana", details = TRUE)
        expect_identical(g$organism_name,"Arabidopsis thaliana")
        
        # test with a second run using locally stored information
        is.genome.available(organism = "Homo sapiens", details = TRUE)
        
        # test with a second run using locally stored information without details
        is.genome.available(organism = "Homo sapiens")
        
        # test whether interface to 'genbank' works properly
        is.genome.available(db = "genbank", organism = "Homo sapiens", details = TRUE)
        
        # test whether it works properly a second time based on the internal files
        is.genome.available(db = "genbank", organism = "Arabidopsis thaliana", details = TRUE)
        
        # test whether it works properly a second time based on the internal files without details
        is.genome.available(db = "genbank", organism = "Arabidopsis thaliana")
        
        # test whether interface to 'ensembl' works properly
        is.genome.available(db = "ensembl", organism = "Homo sapiens", details = TRUE)
        
        # test whether interface to 'ensembl' works properly without details
        is.genome.available(db = "ensembl", organism = "Homo sapiens")
        
        # test whether interface to 'ensemblgenomes' works properly
        is.genome.available(db = "ensemblgenomes", organism = "Arabidopsis thaliana", details = TRUE)
        
        # test whether interface to 'ensemblgenomes' works properly without details
        is.genome.available(db = "ensemblgenomes", organism = "Arabidopsis thaliana")
        
})


test_that("The is.genome.available() error messages work properly..",{
        skip_on_cran()
        
    # test unknown organism error handling for 'refseq'
    expect_identical(is.genome.available(organism = "Hrmo sapi", db = "refseq"), FALSE)
    # test unknown organism error handling for 'genbank'
    expect_identical(is.genome.available(organism = "Hrmo sapi", db = "genbank"), FALSE)
    # test unknown organism error handling for 'ensembl'
    expect_error(is.genome.available(organism = "Hrmo sapi", db = "ensembl"), "Unfortunately organism 'Hrmo sapi' is not available at ENSEMBL. Please check whether or not the organism name is typed correctly.")
})
