context("Test: getCDS()")

test_that("The getCDS() interface to NCBI RefSeq works properly..",{
       
        skip_on_cran()
        read_cds(getCDS( db       = "refseq",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})        

test_that("The getCDS() interface to NCBI Genbank works properly..",{
    
    skip_on_cran()
    read_cds(getCDS( db       = "genbank",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})

test_that("The getCDS() interface to Ensembl works properly..",{
    
    skip_on_cran()
    read_cds(getCDS( db       = "ensembl",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})


test_that("The getCDS() interface to EnsemblGenomes works properly..",{
    
    skip_on_cran()
    read_cds(getCDS( db       = "ensemblgenomes",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})


test_that("The getCDS() error messages work properly..",{
        skip_on_cran()
        
    expect_warning(getCDS( db       = "ensembl",
                        organism = "Saccharomyces cerevisi",
                        path     = tempdir()))
})


test_that("The getCDS() throws error when wrong 'db' is selected..",{
    skip_on_cran()
    
    expect_error(getCDS( db       = "somethingelse",
                           organism = "Saccharomyces cerevisi",
                           path     = tempdir()))
})


test_that("The getCDS() throws error when wrong 'organism' is selected..",{
    skip_on_cran()
    
    expect_message(getCDS( db       = "refseq",
                         organism = "somethingelse",
                         path     = tempdir()))
})












