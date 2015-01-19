getMarts <- function(){
        
        # connect to BioMart API
        biomartPage <- httr::handle("http://www.biomart.org:80/biomart/martservice?type=registry&requestid=biomart")
        xmlContentMarts <- httr::GET(handle = biomartPage)
        
        # test whether or not a connection could be established
        httr::stop_for_status(xmlContentMarts)
        
        # parse Mart information
        doc <- XML::xmlTreeParse(xmlContentMarts, useInternal = TRUE)
        rootNode <- XML::xmlRoot(doc)
        
        # extract databases
        databases <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"database")))
        
        # extract database versions
        displayNames <- as.data.frame(XML::xmlSApply(rootNode, function(x) XML::xmlGetAttr(x,"displayName")))
        
        return(data.frame(mart = databases[ , 1], version = displayNames[ , 1]))
               
}