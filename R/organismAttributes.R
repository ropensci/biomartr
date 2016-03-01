#' @title Retrieve Biomart Attributes for an Organism
#' @description In addition to the \code{\link{organismBM}} function, this function
#' returns all available attributes that can be accessed through different marts and datasets 
#' for a given query organism.
#' @param organism a character string specifying the scientific name of a query organism.
#' @param update a logical value specifying whether or not the local listMart.txt, listDatasets.txt,
#' and listAttributes_organism.txt files shall be updated by remote access to BioMart.
#' @param topic a character string specifying a topic (category) of attributes, e.g. \code{topic} = \code{"id"}.
#' @author Hajk-Georg Drost
#' @return a data.frame storing corresponding attribute names, description, datasets, and marts.
#' @details
#' For a given query organism, this function retrieves all available attributes that 
#' can be accessed through different marts and datasets.
#' 
#' Sometimes the same attribute names correspond to different datasets and marts causing
#' problems when using \code{\link{getMarts}}. The approach introduced by this function
#' provides (again) a organism centric way of accessing organism specific attributes.
#' 
#' The \code{topic} argument allows the user to search for specific attribute topics/categories
#' for faster filtering.
#' @note
#' When you run this function for the first time, the data retrieval procedure will take some time,
#' due to the remote access to BioMart. The corresponding result is then saved in a *.txt file within the \code{\link{tempdir}}
#' directory named "_biomart/listMarts.txt","_biomart/listDatasets.txt", and "_biomart/listAttributes_organism.txt",
#' allowing subsequent queries to perform much faster.
#' @examples \dontrun{
#' 
#' # return available attributes for "Homo sapiens"
#' head(organismAttributes("Homo sapiens"), 20)
#' 
#' # search for attribute topic "id" 
#' head(organismAttributes("Homo sapiens", topic = "id"), 20)
#' 
#' # search for attribute topic "homolog" 
#' head(organismAttributes("Homo sapiens", topic = "homolog"), 20)
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
#' @seealso \code{\link{organismFilters}}, \code{\link{organismBM}}, \code{\link{biomart}},
#' \code{\link[biomaRt]{listAttributes}}
#' @export

organismAttributes <- function(organism, update = FALSE, topic = NULL){
        
        name <- description <- mart <- dataset <- NULL
        
        orgBM <- organismBM(organism = organism, update = update)
        orgMarts <- names(table(orgBM[ , "mart"]))
        martList <- lapply(orgMarts, function(mart) dplyr::filter(orgBM,mart == mart))
        
        fsep <- .Platform$file.sep
        attrTXT <- paste0("listAttributes_",stringr::str_replace(organism," ","_"))
        
        if(!file.exists(file.path(tempdir(),"_biomart"))){
                
                dir.create(file.path(tempdir(),"_biomart"))
        }       
        
        if(!file.exists(file.path(tempdir(),"_biomart",paste0(attrTXT,".txt")))){
                        
                        attrList <- lapply(martList, function(mart) { 
                                
                                mart <- as.data.frame(mart); 
                                mart_tbl <- do.call(rbind,lapply(1:nrow(mart),
                                                                 function(dataset) {
                                                                         
                                                                         attr_tbl <- getAttributes(
                                                                                          dataset = mart[ dataset , "dataset"], 
                                                                                          mart    = mart[ dataset , "mart"])
                                                                         
                                                                         datasetVec <- rep(mart[ dataset , "dataset"], nrow(attr_tbl))
                                                                         
                                                                         attr_tbl <- dplyr::mutate(attr_tbl, dataset = datasetVec)
                                                                         
                                                                         return(attr_tbl)
                                                                 }))
                                
                                martVec <- rep(mart[1  , "mart"], nrow(mart_tbl))
                                mart_tbl <- dplyr::mutate(mart_tbl, mart = martVec)
                                return(mart_tbl)
                        }
                        )
                        
                        utils::write.table(do.call(rbind,attrList),
                                           file.path(tempdir(),"_biomart",paste0(attrTXT,".txt")),
                                           sep       = "\t",
                                           quote     = FALSE,
                                           col.names = TRUE,
                                           row.names = FALSE)
        }
        
        attributeTable <- utils::read.csv(file.path(tempdir(),"_biomart",paste0(attrTXT,".txt")),
                                          sep              = "\t",
                                          header           = TRUE,
                                          colClasses       = rep("character", 4),
                                          stringsAsFactors = FALSE)        
        
#         summ_attrTable <- dplyr::summarise(dplyr::group_by(attributeTable, name),
#                                            description = names(table(description)),
#                                            mart        = names(table(mart)),
#                                            dataset     = names(table(dataset)))
        
        if(!is.null(topic)){
                
                findTopic <- which(sapply(attributeTable[ , "name"],function(x) stringr::str_detect(x,topic)))
                
                if(dim(attributeTable[findTopic , ])[1] == 0)
                        stop("Unfortunately the topic '", topic ,"' could not be found.")
                
                return(attributeTable[findTopic , ])
                
        } else {
                
                return(attributeTable)
        }
}






