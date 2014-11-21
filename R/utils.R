#' @title Helper function for geneSequence()
#' @description This function takes a gene id as character and returns the biological sequence
#' of the corresponding gene id.
#' @param gene a character vector storing the gene id of a organisms of interest to be queried against the 
#' acnucdb database.
#' @author Hajk-Georg Drost
#' @details Sequence information is retrieved from the acnucdb database. 
retrieve_sequence <- function(gene){
        
        query_string <- paste0("AC=",gene)
        try(query("input",query_string))
        gene_sequence <- seqinr::getSequence(input$req[[1]])
        return(gene_sequence)
        
}