#' @title Gene Ontology Query
#' @description This function takes a gene id as character vector from a given query organism and returns the 
#' corresponding GO terms and additional GO information.
#' @param organism a character string specifying the scientific name of a query organism.
#' @param genes a character vector storing the gene ids of a organisms of interest to be queried against BioMart.
#' @param filters a character vector specifying the filter (query key) for the BioMart query, e.g. \code{filter} = \code{"ensembl_gene_id"}.
#' @param database a cahracter string specifying the database from which GO information shall be retrieved.
#' Possible choices are: \code{database} = \code{"BioMart"} and \code{database} = \code{"DAVID"}.
#' @param email a character string specifying the email address for your \code{DAVID} account. This
#' parameter is only used to query \code{DAVID} web services and by default is \code{email} = \code{NULL}.
#' @param ... additional parameters that can be passed to the \code{\link{biomart}} function.
#' @author Hajk-Georg Drost
#' @details 
#' This function takes the scientific name of a query organism, a set of genes for which GO terms
#' and additional information shall be retrieved, and a filter argument that specifies the attribute
#' for the query genes.  
#' 
#' @examples \dontrun{
#' 
#' GO_tbl <- getGO(organism = "Arabidopsis thaliana", 
#'                 genes    = c("AT1G06090", "AT1G06100", 
#'                              "AT1G06110", "AT1G06120", 
#'                              "AT1G06130", "AT1G06200"),
#'                 filters  = "tair_locus")
#' 
#' # look at the result
#' head(GO_tbl[ , c("tair_locus","go_accession","go_name_1006")])
#' 
#' }
#' @seealso \code{\link{biomart}}, \code{\link{organismFilters}}, \code{\link{organismBM}}, \code{\link[biomaRt]{getBM}}, \code{\link{getMarts}},
#' \code{\link{getDatasets}}, \code{\link{getFilters}}
#' @export

getGO <- function(organism, genes, filters, database = "BioMart", email = NULL, ...){
        
        name <- dataset <- NULL
        
        if(!is.element(database,c("BioMart","DAVID")))
                stop("A database named '",database,"' cannot be accesed via getGO().",
                     "Plaese choose from 'BioMart' and 'DAVID'.")
        
        
        if(database == "DAVID"){
                
                if(is.null(email))
                        stop("Please specify a valid email address to be able to query DAVID web services.")
                
                
                
        }
        
        if(database == "BioMart"){
                
                orgAttr <- organismAttributes(organism = organism, topic = "go")
                
                GOattributes <- c("go_accession",
                                  "go_definition_1006",
                                  "go_name_1006", "go_id")
                
                GOattr_df <- dplyr::filter(orgAttr, name %in% GOattributes)
                
                if(dim(GOattr_df)[1] == 0)
                        stop("Unfortunately for '",organism,"' no GO attributes could be found.")
        
                m <- names(table(GOattr_df[ , "mart"]))
                d <- names(table(GOattr_df[ , "dataset"]))
                
                if((length(m) > 1) | (length(d) > 1))
                        stop("GO related attributes have been found in more than one mart or dataset: mart = ",m," ; dataset = ",dataset)

                return( biomart(genes      = genes,
                                mart       = m, 
                                dataset    = d,
                                attributes = GOattributes[GOattr_df[ , "name"] == GOattributes],
                                filters    = filters, ...) )
        
        }
}


