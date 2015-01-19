#' @title Retrieve All Available BioMart Databases
#' @description This funcion queries the BioMart Interface and returns a table
#' storing all available BioMart databases.
#' 
#' @author Hajk-Georg Drost
#' @examples
#' 
#' # get a table of all available databases from BioMart
#' getMarts()
#' 
#' @export
getMarts <- function(){
        
        # connect to BioMart API
        biomartPage <- httr::handle("http://www.biomart.org:80/biomart/martservice?type=registry&requestid=biomart")
        xmlContentMarts <- httr::GET(handle = biomartPage)
        
        # test whether or not a connection could be established
        httr::stop_for_status(xmlContentMarts)
        
        # parse Mart information
        doc <- XML::xmlTreeParse(xmlContentMarts, useInternal = TRUE)
        rootNode <- XML::xmlRoot(doc)
        
        # extract available databases
        databases <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"database")))
        
        # extract available database versions
        displayNames <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"displayName")))
        
        return(data.frame(mart = databases[ , 1], version = displayNames[ , 1]))
               
}