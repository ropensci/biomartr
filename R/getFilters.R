#' @title Retrieve All Available Filters for a Specific Dataset
#' @description This funcion queries the BioMart API and returns a table
#' storing all available filters for a specific dataset.
#' @param mart a character string specifying the database (mart) for which
#' datasets shall be listed.
#' @param dataset a character string specifying the dataset for which filters
#' shall be listed.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # search for available datasets
#' # getMarts()
#' # choose database (mart): "ENSEMBL_MART_ENSEMBL"
#' # head(getDatasets(mart = "ENSEMBL_MART_ENSEMBL"), 10)
#' # choose dataset: "hsapiens_gene_ensembl"
#' head(getFilters(mart = "ENSEMBL_MART_ENSEMBL",
#'                 dataset = "hsapiens_gene_ensembl") , 5)
#' }
#' @seealso \code{\link{getMarts}}, \code{\link{getDatasets}},
#' \code{\link{getAttributes}}, \code{\link{organismBM}},
#' \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export

getFilters <- function(mart, dataset, mute_citation = FALSE) {

    if ((!is.character(mart)) || (!is.character(dataset)))
        stop("Please use a character string as mart or dataset.", call. = FALSE)

    message("Starting retrieval of filters information from mart ", mart, " and dataset ", dataset, " ...")
    all_ensembl_url <- biomart_base_urls()
    index <- unlist(lapply(names(all_ensembl_url), function(x) grep(x, mart, ignore.case = TRUE)))
    if (length(index) != 1) stop(wrong_mart_message(mart, dataset))
    ensembl_url <- all_ensembl_url[index]

    url <- paste0(ensembl_url, "/biomart/martservice?type=filters&dataset=",
         dataset,
         "&requestid=biomart&mart=",
         mart)

    testContent <- httr::content(httr::GET(url), as = "text")

    if ((testContent == "Attribute 'mains' does not exist\n") ||
        is.na(testContent)) {
        warning("No attributes were available for mart = ",
                mart,
                " and dataset = ",
                dataset,
                ".",
                call. = FALSE)
        filterBioMart <- data.frame(name = "NA", description = "NA")
        return(filterBioMart)
    }

    filterPage <- httr::handle(url)
    xmlContentFilters <- httr::GET(handle = filterPage)

    httr::stop_for_status(xmlContentFilters)


    tryCatch({
        # extract attribute name and attribute description
        suppressWarnings(rawDF <-
                             do.call("rbind", apply(as.data.frame(strsplit(
                                 httr::content(xmlContentFilters, as = "text",
                                               encoding = "UTF-8"),
                                 "\n"
                             )), 1, function(x)
                                 unlist(strsplit(x, "\t")))))

        colnames(rawDF) <- paste0("V", seq_len(ncol(rawDF)))

        filterBioMart <-
            as.data.frame(rawDF[, c("V1", "V2")],
                          stringsAsFactors = FALSE,
                          colClasses = rep("character", 2))
        colnames(filterBioMart) <- c("name", "description")

        please_cite_biomartr(mute_citation = mute_citation)

        return(filterBioMart)

    }, error = function(e) stop(wrong_mart_message(mart, dataset)))
}


