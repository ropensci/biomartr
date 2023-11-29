#' @title Helper function to query sub marts from Ensembl Biomart
#' @description Internal interface function to the Ensembl Biomart API
#' @param submart type of submart
#' @author Hajk-Georg Drost
#' @noRd
getSubMarts <- function(submart = "ensembl") {

    url <- biomart_full_url(mart = submart, NULL, "registry", port = 443)
    # Example: "https://fungi.ensembl.org:443/biomart/martservice?type=registry&requestid=biomart"
    biomartPage <- httr::handle(url)

    xmlContentMarts <- httr::GET(handle = biomartPage)

    # test whether or not a connection could be established
    httr::stop_for_status(xmlContentMarts)
    service_unavailable <- length(grep("Service unavailable",
                                       httr::content(xmlContentMarts, as = "text", encoding = "UTF-8"))) > 0
    if (service_unavailable) stop("Ensembl could be reached, but biomart service was unavailable.",
                                  " Most likely heavy traffic, wait 10 seconds and try again.")
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

biomart_base_urls <- function() {
  mart_names <- as.character(ensembl_divisions_short(FALSE))
  # TODO: Is there no way to get bacteria in?
  mart_names <- mart_names[!(mart_names %in% "bacteria")]
  # Create URLs
  marts <- paste0("https://", mart_names, ".ensembl",".org")
  marts[1] <- "https://www.ensembl.org"
  names(marts) <- mart_names
  return(marts)
}

biomart_base_urls_select <- function(mart, dataset = NULL) {
  all_ensembl_url <- biomart_base_urls()
  index <- which(lengths(lapply(names(all_ensembl_url), function(x) grep(x, mart, ignore.case = TRUE))) > 0)
  if (length(index) != 1) stop(wrong_mart_message(mart, dataset))
  ensembl_url <- all_ensembl_url[index]
  return(ensembl_url)
}

#' Get full url for biomart WEB API call
#'
#' Example url:
#' "https://fungi.ensembl.org:443/biomart/martservice?type=registry&requestid=biomart"
#' @noRd
biomart_full_url <- function(mart, dataset, type, port = NULL) {
  ensembl_url <- biomart_base_urls_select(mart, dataset)
  if (!is.null(port)) {
    stopifnot(is.numeric(port))
    ensembl_url <- paste0(ensembl_url, ":", port)
  }
  service_url <- "/biomart/martservice?"
  type_param <- paste0("type=", type)
  dataset_param <- ifelse(!is.null(dataset), paste0("dataset=", dataset), "")
  request_id_param <- "requestid=biomart"
  mart_param <- ifelse(type != "registry", paste0("mart=", mart), "")
  url_parameters <- paste(type_param, dataset_param, request_id_param, mart_param, sep = "&")
  url_parameters <- gsub("&&", "&", url_parameters)
  url_parameters <- gsub("&$", "", url_parameters)
  url <- paste0(ensembl_url, service_url, url_parameters)
  return(url)
}
