context("Test: getGFF()")

test_that("The getGFF() interface works properly for RefSeq..",{
    
    skip_on_cran()

    # test proper download from refseq
    getGFF( db       = "refseq", 
                   organism = "Homo sapiens", 
                   path = file.path("_ncbi_downloads","annotation"))
})    

test_that("The getGFF() interface works properly for Genbank..",{
    # test proper download from genbank
    getGFF( db       = "genbank", 
                   organism = "Homo sapiens", 
                   path = file.path("_ncbi_downloads","annotation"))
})    

test_that("The getGFF() interface works properly for ENSEMBL",{
    # test proper download from ENSEMBL
    getGFF( db       = "ensembl", 
                   organism = "Saccharomyces cerevisiae", 
                   path = file.path("_ncbi_downloads","annotation"))
  
})


