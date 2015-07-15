context("Test: getGO()")


equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The getGO() interface works properly..",{
        
        GO_tbl <- getGO(organism = "Arabidopsis thaliana",
                        genes    = c("AT1G06090", "AT1G06100",
                                     "AT1G06110", "AT1G06120",
                                     "AT1G06130", "AT1G06200"),
                        filters  = "tair_locus")
        
        
        expect_true(equal_lists(as.list(GO_tbl[1 , c("tair_locus","go_accession","go_name_1006")]), list(tair_locus = "AT1G06090",
                                                                                                         go_accession = "GO:0016717", go_name_1006 = "oxidoreductase activity, acting on paired donors, with oxidation of a pair of donors resulting in the reduction of molecular oxygen to two molecules of water")))
})





