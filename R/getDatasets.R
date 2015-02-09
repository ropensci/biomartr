#' @title Retrieve All Available Datasets for a BioMart Database
#' @description This funcion queries the BioMart Interface and returns a table
#' storing all available datasets for a selected BioMart databases.
#' 
#' @param mart a character string specifying the database (mart) for which datasets shall be listed.
#' @author Hajk-Georg Drost
#' @examples
#' 
#' # search for available datasets
#' head(getMarts(), 10)
#' 
#' # choose database: "plants_mart_24"
#' # and get a table of all available datasets from this BioMart database
#' head(getDatasets("plants_mart_24"), 10)
#' 
#' @seealso \code{\link{getMarts}}, \code{\link{getAttributes}}, \code{\link{getFilters}}, \code{\link{organismBM}}, \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export
getDatasets <- function(mart){
        
        if(!is.character(mart))
                stop("Please use a character string as mart.")
        
        datasetPage <- httr::handle(paste0("http://www.biomart.org/biomart/martservice?type=datasets&requestid=biomart&mart=",mart))
        xmlContentDatasets <- httr::GET(handle = datasetPage)
        
        httr::stop_for_status(xmlContentDatasets)
        
        # extract dataset name, description, and version, etc.
        rawDF <- do.call("rbind",apply(as.data.frame(strsplit(httr::content(xmlContentDatasets,as = "text"),"\n")),1,function(x) unlist(strsplit(x,"\t"))))
        
        colnames(rawDF) <- paste0("V",1:ncol(rawDF))
         
        if(dim(rawDF)[1] > 2)
                # store available datasets
                dsBioMart <- as.data.frame(rawDF[-seq(1,nrow(rawDF),2), c("V2","V3","V5")], stringsAsFactors = FALSE, colClasses = rep("character",3))
        
        if(dim(rawDF)[1] <= 2)
                dsBioMart <- data.frame(V2 = "", V3 = "", V5 = "")
        
        colnames(dsBioMart) <- c("dataset","description","version")
        
        return(dsBioMart)
        
}
