#' @title Retrieve Biomart Marts and Datasets for an Organism
#' @description This function returns either all available biomart connections for
#' all available organisms for which biomart access is possible, or (when specified) returns all
#' organism specific biomart connections.
#' @param organism a character string specifying the scientific name of a query organism.
#' Default is \code{organism} = \code{NULL}. In this case all available biomart connections are returned.
#' @param update a logical value specifying whether or not the local listMart.txt and listDatasets.txt files shall be updated
#' by remote access to BioMart. 
#' @author Hajk-Georg Drost
#' @details
#' This function collects all available biomart connections and returns a table storing
#' the organism for which biomart connections are available as well as the corresponding mart and dataset.
#'   
#' 
#' @note
#' When you run this function for the first time, the data retrieval procedure will take some time,
#' due to the remote access to BioMart. The corresponding result is then saved in a *.txt file named
#' "_biomart/listDatasets.txt" allowing subsequent queries to perform much faster.
#' 
#' @examples \dontrun{
#' 
#' # returning all available biomart connections
#' head(organismBM(), 20)
#' 
#' # retrieving all available datasets and biomart connections for
#' # a specific query organism (scientific name)
#' organismBM(organism = "Arabidopsis thaliana")
#' 
#' # you can also update the downloaded version using the "update = TRUE" argument
#' head(organismBM(update = TRUE), 20)
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
#' @seealso \code{\link{getMarts}}, \code{\link{getDatasets}}, \code{\link{biomart}},
#' \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export

organismBM <- function(organism = NULL, update = FALSE){
        
        fsep <- .Platform$file.sep
        
        if(update){
                
                if(file.exists(paste0("_biomart",fsep,"listMarts.txt")))
                        unlink(paste0("_biomart",fsep,"listMarts.txt"))
                
        }
        
        if(!file.exists(paste0("_biomart",fsep,"listMarts.txt"))){
                
                if(!file.exists("_biomart"))
                        dir.create("_biomart")
                
                all_marts <- droplevels.data.frame(getMarts())
                write.table(all_marts,paste0("_biomart",fsep,"listMarts.txt"),sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
                
        }
        
        
        if(file.exists(paste0("_biomart",fsep,"listMarts.txt")))
                all_marts <- read.csv("_biomart/listMarts.txt", header = TRUE, sep = "\t", colClasses = rep("character",2),stringsAsFactors = FALSE)
        
        if(update){
                
                if(file.exists(paste0("_biomart",fsep,"listDatasets.txt")))
                        unlink(paste0("_biomart",fsep,"listDatasets.txt"))
                
        }
        
        
        if(!file.exists(paste0("_biomart",fsep,"listDatasets.txt"))){
                all_datasets <- do.call(rbind,lapply(as.vector(all_marts[ , "mart"]), 
                                       function(mart){ df <- as.data.frame(getDatasets(mart = mart))
                                                       df <- dplyr::mutate(df,mart = rep(mart,length(mart)))
                                                       return(df) } ))
                                                        
                
                write.table(all_datasets,paste0("_biomart",fsep,"listDatasets.txt"),sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
        }
        
        if(file.exists(paste0("_biomart",fsep,"listDatasets.txt")))
                all_datasets <- read.csv(paste0("_biomart",fsep,"listDatasets.txt"), header = TRUE, sep = "\t", colClasses = rep("character",3),stringsAsFactors = FALSE)
        
  
       all_datasets <- dplyr::mutate(all_datasets,organism_name = sapply(all_datasets[ ,"description"], function(x) paste0(strsplit(x," ")[[1]][1:2],collapse = " ")))
       all_datasets <- dplyr::select(all_datasets, organism_name,description,mart,dataset,version)
          
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





