#' @title Helper function to query sub marts from Ensembl Biomart
#' @description Internal interface function to the Ensembl Biomart API
#' @param submart type of submart
#' @author Hajk-Georg Drost
#' @noRd
getSubMarts <- function(submart = "ensembl") {
    
    if (!is.element(submart, c("ensembl", "plants", "fungi", 
                               "protists", "metazoa")))
        stop(
            "Please select a submart that is supported by ensembl: 
            submart = 'ensembl', submart = 'plants', submart = 'fungi',  
            submart = 'protists', or submart = 'metazoa'",
            call. = FALSE
        )
    
    if (submart == "ensembl")
        # connect to BioMart API
        biomartPage <- httr::handle(
"http://www.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart"
        )
    
    if (submart == "plants")
        # connect to BioMart API
        biomartPage <- httr::handle(
"http://plants.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart"
        )
    
    if (submart == "fungi")
        # connect to BioMart API
        biomartPage <- httr::handle(
"http://fungi.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart"
        )
    
    if (submart == "protists")
        # connect to BioMart API
        biomartPage <- httr::handle(
"http://protists.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart"
        )
    
    if (submart == "metazoa")
        # connect to BioMart API
        biomartPage <- httr::handle(
"http://metazoa.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart"
        )

    xmlContentMarts <- httr::GET(handle = biomartPage)
    
    # test whether or not a connection could be established
    httr::stop_for_status(xmlContentMarts)
    
    # parse Mart information
    doc <- suppressMessages(XML::xmlTreeParse(
        xmlContentMarts,
        useInternalNodes = TRUE,
        encoding = "UTF-8"
    ))
    rootNode <- XML::xmlRoot(doc)
    
    # extract available databases
    databases <- as.data.frame(XML::xmlSApply(rootNode, function(x)
        XML::xmlGetAttr(x, "name")))
    
    # extract available database versions
    displayNames <- as.data.frame(XML::xmlSApply(rootNode, function(x)
        XML::xmlGetAttr(x, "displayName")))
    
    # ectract information whether or not the corresponding database is visible
    visible <- as.data.frame(XML::xmlSApply(rootNode, function(x)
        XML::xmlGetAttr(x, "visible")))
    
    dbBioMart <- tibble::tibble(
        mart = as.character(databases[, 1]),
        version = as.character(displayNames[, 1]),
        visible = as.character(visible[, 1])
    )
    
    mart <- version <- NULL
    
    dbBioMart <- dplyr::select(dplyr::filter(dbBioMart, visible != "0"), 
                               mart, version)
    
    return(dbBioMart)
}
