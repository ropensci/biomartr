context("Test: getGFF()")

test_that("The getGFF() interface works properly for NCBI RefSeq (repeating command)..",{
    
        skip_on_cran()
        skip_on_travis()
    # test proper download from refseq
    getGFF( db       = "refseq", 
            organism = "Saccharomyces cerevisiae", 
            path = tempdir())
    
    getGFF( db       = "refseq", 
            organism = "Saccharomyces cerevisiae", 
            path = tempdir())
})

test_that("The getGFF() interface works properly for NCBI RefSeq using taxid (repeating command)..",{
    
    skip_on_cran()
    skip_on_travis()
    # test proper download from refseq
    getGFF( db       = "refseq", 
            organism = "559292", 
            path = tempdir())
    
    getGFF( db       = "refseq", 
            organism = "559292", 
            path = tempdir())
})

test_that("The getGFF() interface works properly for NCBI Genbank (repeating command)..",{
        skip_on_travis()
        skip_on_cran()
    # test proper download from genbank
    getGFF( db       = "genbank", 
            organism = "Saccharomyces cerevisiae", 
            path = tempdir())
    
    getGFF( db       = "genbank", 
            organism = "Saccharomyces cerevisiae", 
            path = tempdir())
})    


test_that("The getGFF() interface works properly for Ensembl (repeating command)",{
        skip_on_cran()
        skip_on_travis()

    # test proper download from Ensembl
    getGFF( db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())
    
    getGFF( db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path = tempdir())
})
