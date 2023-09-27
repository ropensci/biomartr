#' @title Helper function to query sub marts from Ensembl Biomart
#' @description Internal interface function to the Ensembl Biomart API
#' @param submart type of submart
#' @author Hajk-Georg Drost
#' @noRd
getSubMarts <- function(submart = "ensembl") {

    mart_names <- as.character(ensembl_divisions_short(FALSE))
    # TODO: Is there no way to get bacteria in?
    mart_names <- mart_names[!(mart_names %in% "bacteria")]

    if (!is.element(submart, mart_names))
        stop(
            "Please select a submart that is supported by ensembl:\n",
            paste(mart_names, collapse = ", "),
            call. = FALSE
        )
    # Create URLs
    marts <- paste0("http://", mart_names, ".ensembl",".org")
    marts[1] <- "http://www.ensembl.org"
    port_id <- ":80/biomart/martservice?"
    request_url <- "type=registry&requestid=biomart"
    marts <- paste0(marts, port_id, request_url)
    names(marts) <- mart_names
    # Example: "http://fungi.ensembl.org:80/biomart/martservice?type=registry&requestid=biomart"

    biomartPage <- httr::handle(marts[submart])

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
        mart = as.character(databases[ , 1]),
        version = as.character(displayNames[ , 1]),
        visible = as.character(visible[ , 1])
    )

    mart <- version <- NULL

    dbBioMart <- dplyr::select(dplyr::filter(dbBioMart, visible != "0"),
                               mart, version)

    return(dbBioMart)
}
