#' @title Retrieve All Available BioMart Databases
#' @description This funcion queries the BioMart API and returns a table
#' storing all available BioMart databases.
#' 
#' @author Hajk-Georg Drost
#' @examples
#' 
#' \dontrun{
#' # get a table of all available databases from BioMart
#'  getMarts()
#' }
#' 
#' @seealso \code{\link{getDatasets}}, \code{\link{getAttributes}}, \code{\link{getFilters}}, \code{\link{organismBM}}, \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export
getMarts <- function(){
        
        # connect to BioMart API
        biomartPage <- httr::handle("http://www.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart")
        xmlContentMarts <- httr::GET(handle = biomartPage)
        
        # test whether or not a connection could be established
        httr::stop_for_status(xmlContentMarts)
        
        # parse Mart information
        doc <- XML::xmlTreeParse(xmlContentMarts, useInternal = TRUE)
        rootNode <- XML::xmlRoot(doc)
        
        # extract available databases
        databases <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"name")))
        
        # extract available database versions
        displayNames <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"displayName")))
        
        # ectract information whether or not the corresponding database is visible 
        visible <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"visible")))
        
        dbBioMart <- data.frame(mart = databases[ , 1], version = displayNames[ , 1], visible = visible[ , 1])
        
        return(dplyr::filter(dbBioMart, visible != "0")[ , c("mart","version")])
               
}






