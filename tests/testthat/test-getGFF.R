context("Test: getGFF()")

test_that("The getGFF() interface works properly for NCBI RefSeq..",{
    
        skip_on_cran()
        skip_on_travis()
        Sys.sleep(1)
    # test proper download from refseq
    getGFF( db       = "refseq", 
            organism = "Saccharomyces cerevisiae", 
            path = file.path("_ncbi_downloads","annotation"))
})    

test_that("The getGFF() interface works properly for NCBI Genbank..",{
        skip_on_travis()
        skip_on_cran()
        Sys.sleep(1)
    # test proper download from genbank
    getGFF( db       = "genbank", 
            organism = "Saccharomyces cerevisiae", 
            path = file.path("_ncbi_downloads","annotation"))
})    


# test_that("The getGFF() interface works properly for Ensembl",{
#         skip_on_cran()
#         skip_on_travis()
#         Sys.sleep(1)
#     # test proper download from Ensembl
#     getGFF( db       = "ensembl", 
#             organism = "Saccharomyces cerevisiae", 
#             path = file.path("ensembl","annotation"))
# })
# 
# test_that("The getGFF() interface works properly for EnsemblGenomes",{
#         skip_on_cran()
#         skip_on_travis()
#         Sys.sleep(1)
#     # test proper download from EnsemblGenomes
#     getGFF( db       = "ensemblgenomes", 
#             organism = "Saccharomyces cerevisiae", 
#             path = file.path("ensembl","annotation"))
# })
