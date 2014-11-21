#' @title Function to retrieve GO terms for a given set of genes
#' @description This function takes a gene id as character vector and returns the 
#' corresponding GO term.
#' @param ...
#' @author Hajk-Georg Drost
#' @details GO. 
#' @examples \dontrun{
#' getGO(genes =c("AT1G06090", "AT1G06100", "AT1G06110", "AT1G06120", "AT1G06130", "AT1G06200"),
#' mart = "plants_mart_22",dataset = "athaliana_eg_gene",
#' attributes = c("go_accession","go_definition_1006","go_name_1006"),
#' filters = "tair_locus",uniqueRows = FALSE)
#' 
#' }
#' @export
getGO <- function(...){
        
        return( biomart( ... ) )
}


