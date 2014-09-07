#' @title Main function to query the biomart database
#' @description This function takes a set of gene ids and the biomart specifications
#' and performs a biomart query for the given set of gene ids.
#' @param genes a character vector storing the gene ids of a organisms of interest to be queried against biomart.
#' @param mart a character string specifying the mart to be used, e.g. mart = "ensembl".
#' @param dataset a character string specifying the dataset within the specified mart that shall be used, e.g. dataset = "hsapiens_gene_ensembl".
#' @param attributes
#' @param filter
#' @param ... 
#' @author Hajk-Georg Drost
#' @export
biomart <- function(genes,mart,dataset,attributes,filter,...){
        m <- useMart(mart)
        d <- useDataset(dataset,mart = m)
        
        ### establishing a biomaRt connection and retrieving the information for the given gene list
        query <- getBM(attributes = attributes,filters = filter, values = genes, mart = d, ...)
        
        # sometimes the query order is not returned correctly by biomart,
        # therefore the query order is being checked 
        inter <- na.omit(match(query[ , 1], genes))
        ### returning a data.frame storing the input gene id's and their corresponding attributes
        return(data.frame(genes[inter],query[ , 2:ncol(query)]))
        
}