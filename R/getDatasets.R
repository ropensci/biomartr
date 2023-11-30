#' @title Retrieve All Available Datasets for a BioMart Database
#' @description This funcion queries the BioMart API and returns a table
#' storing all available datasets for a selected BioMart databases.
#' @param mart a character string specifying the database (mart) for
#' which datasets shall be listed.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # search for available datasets
#' # getMarts()
#' # choose database: "ENSEMBL_MART_ENSEMBL"
#' head(getDatasets("ENSEMBL_MART_ENSEMBL"), 10)
#' }
#' @family biomaRt
#' @export
getDatasets <- function(mart, mute_citation = FALSE) {
    if (!is.character(mart))
        stop("Please use a character string as mart.", call. = FALSE)

  type <- "datasets"
  dataset <- NULL
  dataset_message <- ifelse(!is.null(dataset), paste("and dataset", dataset), "")
  message("Starting retrieval of ", type, " information from mart ", mart, dataset, " ...")
  url <- biomart_full_url(mart, dataset, type)

  datasetPage <- httr::handle(url)
  xmlContentDatasets <- httr::GET(handle = datasetPage)

  tryCatch({
      httr::stop_for_status(xmlContentDatasets)

      # extract dataset name, description, and version, etc.
      rawDF <-
          do.call("rbind", apply(as.data.frame(stringr::str_split(
              httr::content(
                  xmlContentDatasets,
                  as = "text",
                  encoding = "UTF-8"
              ),
              "\n"
          )), 1, function(x)
              unlist(strsplit(x, "\t"))))

      colnames(rawDF) <- paste0("V", seq_len(ncol(rawDF)))

      if (dim(rawDF)[1] > 2)
          # store available datasets
          dsBioMart <-
          as.data.frame(
              rawDF[-seq(1, nrow(rawDF), 2), c("V2", "V3", "V5")],
              stringsAsFactors = FALSE,
              colClasses = rep("character", 3)
          )

      if (dim(rawDF)[1] <= 2)
          dsBioMart <- data.frame(V2 = "",
                                  V3 = "",
                                  V5 = "")

      colnames(dsBioMart) <- c("dataset", "description", "version")
      please_cite_biomartr(mute_citation = mute_citation)

      return(tibble::as_tibble(dsBioMart))
  }, error = function(e)
      stop(wrong_mart_message(mart, dataset)))
}





