#' @title Function to check for available biomart connections for a query organism
#' @description This function returns either all available biomart connections for
#' all available organisms for which biomart access is possible, or (when specified) returns all
#' organism specific biomart connections.
#' @param organism a character string specifying the scientific name of a query organism.
#' Default is \code{organism} = \code{NULL}. In this case all available biomart connections are returned.
#' @author Hajk-Georg Drost
#' @details
#' This function collects all available biomart connections and returns a table storing
#' the organism for which biomart connections are available as well as the corresponding mart and dataset.
#'   
#' 
#' @note
#' When running the function for the first time, data retrieval will take a while,
#' due to the remote access to biomart. The result is then saved in a *.txt file named
#' "_biomart/listDatasets.txt" allowing subsequent queries to perform much faster.
#' 
#' @examples \dontrun{
#' 
#' # returning all available biomart connections
#' head(biomart_organisms(), 20)
#' 
#' # retrieving all available datasets and biomart connections for
#' # a specific query organism (scientific name)
#' biomart_organisms(organism = "Arabidopsis thaliana")
#' 
#' # you can also update the downloaded version using the "update = TRUE" argument
#' head(biomart_organisms(update = TRUE), 20)
#' 
#' }
#' @references
#' 
#' \url{http://biomart.org/}
#' 
#' Mapping identifiers for the integration of genomic datasets with the
#' R/Bioconductor package biomaRt. Steffen Durinck, Paul T. Spellman, Ewan
#' Birney and Wolfgang Huber, Nature Protocols 4, 1184-1191 (2009).
#' 
#' BioMart and Bioconductor: a powerful link between biological databases and
#' microarray data analysis. Steffen Durinck, Yves Moreau, Arek Kasprzyk, Sean
#' Davis, Bart De Moor, Alvis Brazma and Wolfgang Huber, Bioinformatics 21,
#' 3439-3440 (2005).
#' @seealso \code{\link[biomaRt]{listMarts}}, \code{\link[biomaRt]{listDatasets}}, \code{\link{biomart}}
#' @export

biomart_organisms <- function(organism = NULL, update = FALSE){
        
        
        if(!file.exists("_biomart/listMarts.txt")){
                
                if(!file.exists("_biomart"))
                        dir.create("_biomart")
                
                all_marts <- droplevels.data.frame(biomaRt::listMarts())
                write.table(all_marts,"_biomart/listMarts.txt",sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
                
        }
        
        if(update){
                
                if(file.exists("_biomart/listMarts.txt"))
                        unlink("_biomart/listMarts.txt")
                
        }
        
        if(file.exists("_biomart/listMarts.txt"))
                all_marts <- read.csv("_biomart/listMarts.txt", header = TRUE, sep = "\t", colClasses = rep("character",2),stringsAsFactors = FALSE)
        
        if(update){
                
                if(file.exists("_biomart/listDatasets.txt"))
                        unlink("_biomart/listDatasets.txt")
                
        }
        
        if(!file.exists("_biomart/listDatasets.txt")){
                all_datasets <- do.call(rbind,lapply(as.vector(all_marts[ , "biomart"]), 
                                       function(mart){ df <- as.data.frame(biomaRt::listDatasets(biomaRt::useMart(biomart = mart)))
                                                       df <- dplyr::mutate(df,mart = rep(mart,length(mart)))
                                                       return(df) } ))
                                                        
                
                write.table(all_datasets,"_biomart/listDatasets.txt",sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
        }
        
        if(file.exists("_biomart/listDatasets.txt"))
                all_datasets <- read.csv("_biomart/listDatasets.txt", header = TRUE, sep = "\t", colClasses = rep("character",3),stringsAsFactors = FALSE)
        
  
       all_datasets <- dplyr::mutate(all_datasets,organism_name = sapply(all_datasets[ ,"description"], function(x) paste0(strsplit(x," ")[[1]][1:2],collapse = " ")))
       all_datasets <- dplyr::select(all_datasets, list(organism_name,description,mart,dataset,version))
          
       if(!is.null(organism)){
               
               res <- dplyr::filter(all_datasets,organism_name == organism)
               
               if(dim(res)[1] == 0){
                       stop("Unfortunately, no entry for '", organism, "' has been found.")
               } else {
                       
                       return(dplyr::filter(all_datasets,organism_name == organism))
               }
              
       }
       
       if(is.null(organism))
               return(all_datasets)
             
}





