#' @title Retrieve Ensembl Biomart marts and datasets for a query organism
#' @description This function returns either all available biomart connections
#' for all available organisms for which biomart access is possible, or
#' (when specified) returns all organism specific biomart connections.
#' @param organism a character string specifying the scientific name of a
#' query organism. Default is \code{organism} = \code{NULL}. In this case all
#' available biomart connections are returned.
#' @param update a logical value specifying whether or not the local
#' listMart.txt and listDatasets.txt files shall be updated by remote access
#'  to BioMart.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @details
#' This function collects all available biomart connections and returns a table
#' storing the organism for which biomart connections are available as well as
#' the corresponding mart and database.
#' @note
#' When you run this function for the first time, the data retrieval procedure
#' will take some time, due to the remote access to BioMart. The corresponding
#' result is then saved in a *.txt file named "_biomart/listDatasets.txt" in the
#' \code{\link{tempdir}} directory, allowing subsequent queries to perform
#'  much faster.
#' @examples
#' \dontrun{
#' # returning all available biomart connections
#' head(organismBM(), 20)
#' # retrieving all available datasets and biomart connections for
#' # a specific query organism (scientific name)
#' organismBM(organism = "Homo sapiens")
#' # you can also update the downloaded version using
#' # the "update = TRUE" argument
#' head(organismBM(update = TRUE), 20)
#' }
#' @references
#'
#' \url{https://biomart.org/}
#'
#' Mapping identifiers for the integration of genomic datasets with the
#' R/Bioconductor package biomaRt. Steffen Durinck, Paul T. Spellman, Ewan
#' Birney and Wolfgang Huber, Nature Protocols 4, 1184-1191 (2009).
#'
#' BioMart and Bioconductor: a powerful link between biological databases and
#' microarray data analysis. Steffen Durinck, Yves Moreau, Arek Kasprzyk, Sean
#' Davis, Bart De Moor, Alvis Brazma and Wolfgang Huber, Bioinformatics 21,
#' 3439-3440 (2005).
#' @family biomaRt
#' @export
organismBM <- function(organism = NULL, update = FALSE, mute_citation = TRUE) {
    organism_name <- description <- mart <- dataset <- NULL

    message("Starting retrieval of all available BioMart datasets for ", organism, " ...")
    if (update) {
        if (file.exists(getTMPFile(file.path("_biomart", "listMarts.txt"))))
            unlink(getTMPFile(file.path("_biomart", "listMarts.txt")))

    }

    if (!file.exists(getTMPFile(file.path("_biomart", "listMarts.txt")))) {
        if (!file.exists(file.path(tempdir(), "_biomart")))
            dir.create(file.path(tempdir(), "_biomart"))

        all_marts <- droplevels.data.frame(getMarts())
        readr::write_tsv(
            all_marts,
            file.path(tempdir(), "_biomart", "listMarts.txt"),
            col_names = TRUE
        )

    }

    if (file.exists(getTMPFile(file.path("_biomart", "listMarts.txt"))))
        all_marts <-
            as.data.frame(readr::read_tsv(
                file.path(tempdir(), "_biomart", "listMarts.txt"),
                col_types = readr::cols(
                    "mart" = readr::col_character(),
                    "version" = readr::col_character()
                )
            ))

    if (update) {
        if (file.exists(file.path(tempdir(), "_biomart", "listDatasets.txt")))
            unlink(file.path(tempdir(), "_biomart", "listDatasets.txt"))

    }

    if (!file.exists(file.path(tempdir(), "_biomart", "listDatasets.txt"))) {
        remove.corrupt.marts <- which(all_marts[ , "mart"] %in% c("pride", "metazoa_variations"))

        if (length(remove.corrupt.marts) > 0) {
            all_marts <- all_marts[-remove.corrupt.marts , ]
        }

        message("Datasets for the following marts will be retrieved:")
        print(all_marts)


        all_datasets <-
            do.call(rbind, lapply(unlist(all_marts[ , "mart"]),
                                  function(mart) {
                                    message("Processing mart ", mart, " ...")
                                      df <- getDatasets(mart = mart, mute_citation = TRUE)
                                      df <-
                                          dplyr::mutate(tibble::as_tibble(df),
                                                mart = rep(mart, nrow(df)))
                                      return(df)
                                  }))

        readr::write_tsv(
            all_datasets,
            file.path(tempdir(), "_biomart", "listDatasets.txt"),
            col_names = TRUE
        )
    }

    if (file.exists(file.path(tempdir(), "_biomart", "listDatasets.txt")))
        all_datasets <-
            readr::read_tsv(
                file.path(tempdir(), "_biomart", "listDatasets.txt"),
                col_types = readr::cols(
                    "dataset" = readr::col_character(),
                    "description" = readr::col_character(),
                    "version" = readr::col_character(),
                    "mart" = readr::col_character()
                )
            )

    all_datasets <- dplyr::mutate(all_datasets,
                                  organism_name =
                               unlist(lapply(unlist(all_datasets[ , "dataset"]),
                               function(x)
                               unlist(strsplit(x, "_")[[1]][1]))))

    all_datasets <-
        dplyr::select(all_datasets,
                      organism_name,
                      description,
                      mart,
                      dataset,
                      version)

    if (!is.null(organism)) {
        organism <- unlist(stringr::str_split(organism, " "))
        organism <-
      stringr::str_c(stringr::str_to_lower(stringr::str_sub(organism[1], 1, 1)),
                           organism[2],
                           collapse = "")

        res <-
            dplyr::filter(all_datasets,
                          stringr::str_detect(organism_name, organism))

        if (dim(res)[1] == 0) {
            stop("Unfortunately, no entry for '",
                 organism,
                 "' has been found.")
        } else {
          please_cite_biomartr(mute_citation = mute_citation)

            return(dplyr::filter(
                all_datasets,
                stringr::str_detect(organism_name, organism)
            ))
        }
    }

    message("Retrieval finished successfully.")
    if (is.null(organism))
        return(all_datasets)
}





