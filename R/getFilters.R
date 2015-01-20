#' @title Retrieve All Available Filters for a Specific Dataset
#' @description This funcion queries the BioMart Interface and returns a table
#' storing all available filters for a specific dataset.
#' 
#' @param mart a character string specifying the database (mart) for which datasets shall be listed.
#' @param dataset a character string specifying the dataset for which filters shall be listed.
#' @author Hajk-Georg Drost
#' @examples
#' 
#' # search for available datasets
#' head(getMarts(), 10)
#' 
#' # choose database (mart): "plants_mart_24"
#' # and get a table of all available datasets from this BioMart database
#' head(getDatasets(mart = "plants_mart_24"), 10)
#' 
#' # choose dataset: "athaliana_eg_gene"
#' head(getFilters(mart = "plants_mart_24", dataset = "athaliana_eg_gene") , 5)
#' 
#' @seealso \code{\link{getMarts}}, \code{\link{getDatasets}}, \code{\link{getAttributes}}, \code{\link{organismBM}}, \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export
getFilters <- function(mart, dataset){
        
        if((!is.character(mart)) || (!is.character(dataset)))
                stop("Please use a character string as mart or dataset.")
        
        url <- paste0("http://www.biomart.org/biomart/martservice?type=filters&dataset=",dataset,"&requestid=biomart&mart=",mart,"&virtualSchema=default")
        
        filterPage <- httr::handle(url)
        xmlContentFilters <- httr::GET(handle = filterPage)
        
        httr::stop_for_status(xmlContentFilters)
        
        # extract attribute name and attribute description
        suppressWarnings(rawDF <- do.call("rbind",apply(as.data.frame(strsplit(httr::content(xmlContentFilters,as = "text"),"\n")),1,function(x) unlist(strsplit(x,"\t")))))
        
        colnames(rawDF) <- paste0("V",1:ncol(rawDF))
        
        filterBioMart <- as.data.frame(rawDF[ , c("V1","V2")], stringsAsFactors = FALSE, colClasses = rep("character",2))
        colnames(filterBioMart) <- c("name","description")
        
        return(filterBioMart)
}












