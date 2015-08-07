context("Test: biomart()")

equal_lists <- function(df1, df2) {
        isTRUE(all.equal(df1, df2))
}


test_that("The biomart() interface works properly..",{
        
        skip_on_cran()
        
        marts <- getMarts()[ , "mart"]
        plant_mart <- which(stringr::str_match(marts, "plants_mart_") != "NA")
        
        expect_true(equal_lists(as.list(biomart(genes      = c("AT1G06090", "AT1G06100",
                               "AT1G06110", "AT1G06120",
                               "AT1G06130", "AT1G06200"),
                mart       = as.character(marts[plant_mart]),
                dataset    = "athaliana_eg_gene",
                attributes = c("start_position","end_position","description"),
                filters    = "tair_locus")[1, 1:3]), list(tair_locus = "AT1G06090",
                                                       start_position = 1847883,
                                                       end_position = 1849693)))
        
})