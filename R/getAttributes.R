#' @title Retrieve All Available Attributes for a Specific Dataset
#' @description This function queries the BioMart Interface and returns a table
#' storing all available attributes for a specific dataset.
#'
#' @param mart a character string specifying the database (mart)
#' for which datasets shall be listed.
#' @param dataset a character string specifying the dataset for which
#' attributes shall be listed.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # search for available datasets
#' getMarts()
#'
#' # choose database (mart): ENSEMBL_MART_ENSEMBL
#' # and get a table of all available datasets from this BioMart database
#' head(getDatasets(mart = "ENSEMBL_MART_ENSEMBL"), 10)
#'
#' # choose dataset: "hsapiens_gene_ensembl"
#' head(getAttributes(mart = "ENSEMBL_MART_ENSEMBL",
#'                    dataset = "hsapiens_gene_ensembl") , 5)
#' }
#' @family biomaRt
#' @export
getAttributes <- function(mart, dataset, mute_citation = FALSE){

    if ((!is.character(mart)) || (!is.character(dataset)))
        stop("Please use a character string as mart or dataset.",
             call. = FALSE)

    if (!is.element(mart, getMarts()$mart))
        stop("Please select a valid mart with getMarts().", call. = FALSE)

  type <- "attributes"
  message("Starting retrieval of ", type, " information from mart ", mart, " and dataset ", dataset, " ...")
  url <- biomart_full_url(mart, dataset, type)


    testContent <-
        httr::content(httr::GET(url), as = "text", encoding = "UTF-8")
    if (testContent == "Attribute 'mains' does not exist\n") {
        warning("No attributes were available for mart = ",
                mart,
                " and dataset = ",
                dataset,
                ".",
                call. = FALSE)
        attrBioMart <- data.frame(name = "NA", description = "NA")
        return(attrBioMart)
    }

    attributesPage <- httr::handle(url)
    xmlContentAttributes <- httr::GET(handle = attributesPage)

    httr::stop_for_status(xmlContentAttributes)

    # extract attribute name and attribute description
    suppressWarnings(rawDF <-
                         do.call("rbind", apply(as.data.frame(strsplit(
                             httr::content(xmlContentAttributes, as = "text",
                                           encoding = "UTF-8"),
                             "\n"
                         )), 1, function(x)
                             unlist(strsplit(x, "\t")))))

    colnames(rawDF) <- paste0("V", seq_len(ncol(rawDF)))

    attrBioMart <-
        as.data.frame(rawDF[, c("V1", "V2")],
                      stringsAsFactors = FALSE,
                      colClasses = rep("character", 2))
    colnames(attrBioMart) <- c("name", "description")

    please_cite_biomartr(mute_citation = mute_citation)

    return(attrBioMart)
}











