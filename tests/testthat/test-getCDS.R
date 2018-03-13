context("Test: getCDS()")

test_that("The getCDS() interface to NCBI RefSeq works properly..",{
       
        skip_on_cran()
        skip_on_travis()
        read_cds(getCDS( db       = "refseq",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})        

test_that("The getCDS() interface to NCBI Genbank works properly..",{
    
    skip_on_cran()
    skip_on_travis()
    read_cds(getCDS( db       = "genbank",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})

test_that("The getCDS() interface to Ensembl works properly..",{
    
    skip_on_cran()
    skip_on_travis()
    read_cds(getCDS( db       = "ensembl",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})


test_that("The getCDS() interface to EnsemblGenomes works properly..",{
    
    skip_on_cran()
    skip_on_travis()
    read_cds(getCDS( db       = "ensemblgenomes",
                     organism = "Saccharomyces cerevisiae",
                     path     = tempdir()), format = "fasta")
    
})


test_that("The getCDS() error messages work properly..",{
        skip_on_cran()
        skip_on_travis()
    expect_warning(getCDS( db       = "ensembl",
                        organism = "Saccharomyces cerevisi",
                        path     = tempdir()))
})


test_that("The getCDS() throws error when wrong 'db' is selected..",{
    skip_on_cran()
    skip_on_travis()
    expect_error(getCDS( db       = "somethingelse",
                           organism = "Saccharomyces cerevisi",
                           path     = tempdir()))
})


test_that("The getCDS() throws error when wrong 'organism' is selected..",{
    skip_on_cran()
    skip_on_travis()
    expect_message(getCDS( db       = "refseq",
                         organism = "somethingelse",
                         path     = tempdir()))
})



test_that("getCDS() interface to EnsemblGenomes works with taxid..",{
        
        skip_on_cran()
        skip_on_travis()
        read_cds(getCDS( db       = "ensemblgenomes",
                         organism = "559292",
                         path     = tempdir()), format = "fasta")
        
})


test_that("getCDS() interface to EnsemblGenomes works with accession id..",{
        
        skip_on_cran()
        skip_on_travis()
        read_cds(getCDS( db       = "ensemblgenomes",
                         organism = "GCA_000146045.2",
                         path     = tempdir()), format = "fasta")
        
})

test_that("getCDS() interface to EnsemblGenomes works with taxid..",{
        
        skip_on_cran()
        skip_on_travis()
        read_cds(getCDS( db       = "ensembl",
                         organism = "559292",
                         path     = tempdir()), format = "fasta")
        
})


test_that("getCDS() interface to EnsemblGenomes works with accession id..",{
        
        skip_on_cran()
        skip_on_travis()
        read_cds(getCDS( db       = "ensembl",
                         organism = "GCA_000146045.2",
                         path     = tempdir()), format = "fasta")
        
})




