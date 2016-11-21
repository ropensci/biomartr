context("Test: getGFF()")

test_that("The getGenome() interface works properly..",{
    
    skip_on_cran()
    skip_on_travis() 
    
    # test proper download from refseq
    getGFF( db       = "refseq", 
                   organism = "Homo sapiens", 
                   path = file.path("_ncbi_downloads","annotation"))
    
    # test proper use of internal referece files when command is repeated
    
    # test proper download from genbank
    getGFF( db       = "genbank", 
                   organism = "Homo sapiens", 
                   path = file.path("_ncbi_downloads","annotation"))
    
    # test proper use of internal referece files when command is repeated
    getGFF( db       = "refseq", 
                   organism = "Homo sapiens", 
                   path = file.path("_ncbi_downloads","annotation"))
    
    # test proper download from genbank
    getGFF( db       = "genbank", 
                   organism = "Homo sapiens", 
                   path = file.path("_ncbi_downloads","annotation"))
    
    # test proper download from ENSEMBL
    getGFF( db       = "ensembl", 
                   organism = "Saccharomyces cerevisiae", 
                   path = file.path("_ncbi_downloads","annotation"))
  
})


test_that("The getGFF() error messages work properly..",{
    
    expect_true(getGFF( db       = "ensembl",
                            organism = "Saccharomyces cerevisi",
                            path     = tempdir()))
})
