#' @title Retrieve Ensembl Biomart filters for a qyery organism
#' @description In addition to the \code{\link{organismBM}} and 
#' \code{\link{organismAttributes}} functions, this function
#' returns all available filters that can be accessed through different marts 
#' and datasets for a given query organism.
#' @param organism a character string specifying the scientific name of a 
#' query organism.
#' @param update a logical value specifying whether or not the local 
#' listMart.txt, listDatasets.txt, and listFilters_organism.txt files shall be 
#' updated by remote access to BioMart.
#' @param topic a character string specifying a topic (category) of filters, 
#' e.g. \code{topic} = \code{"id"}.
#' @author Hajk-Georg Drost
#' @return a data.frame storing corresponding filter names, description, 
#' datasets, and marts.
#' @details
#' For a given query organism, this function retrieves all available 
#' filters that can be accessed through different marts and datasets.
#' 
#' Sometimes the same filter names correspond to different datasets and 
#' marts causing problems when using \code{\link{getMarts}}. 
#' The approach introduced by this function provides (again) a organism centric 
#' way of accessing organism specific filters.
#' 
#' The \code{topic} argument allows the user to search for specific filters 
#' topics/categories for faster selection.
#' @note
#' When you run this function for the first time, the data retrieval procedure 
#' will take some time, due to the remote access to BioMart. The corresponding 
#' result is then saved in a *.txt file within the \code{\link{tempdir}}
#' directory  named "_biomart/listMarts.txt","_biomart/listDatasets.txt", and 
#' "_biomart/listFilters_organism.txt", allowing subsequent queries to perform 
#' much faster.
#' @examples 
#' # return available filters for "Homo sapiens"
#' head(organismFilters("Homo sapiens"), 20)
#' # search for filter topic "id" 
#' head(organismFilters("Homo sapiens", topic = "id"), 20)
#' @references
#' \url{http://biomart.org/}
#' 
#' Mapping identifiers for the integration of genomic datasets with the
#' R/Bioconductor package biomaRt. Steffen Durinck, Paul T. Spellman, Ewan
#' Birney and Wolfgang Huber, Nature Protocols 4, 1184-1191 (2009).
#' 
#' BioMart and Bioconductor: a powerful link between biological databases and
#' microarray data analysis. Steffen Durinck, Yves Moreau, Arek Kasprzyk, Sean
#' Davis, Bart De Moor, Alvis Brazma and Wolfgang Huber, Bioinformatics 21,
#' 3439-3440 (2005).
#' @seealso \code{\link{organismBM}}, \code{\link{organismAttributes}}, 
#' \code{\link{getAttributes}}, \code{\link{getDatasets}}, 
#' \code{\link{getMarts}} 
#' @export

organismFilters <- function(organism,
                            update = FALSE,
                            topic = NULL) {
    mart <- dataset <- NULL
    
    orgBM <- organismBM(organism = organism, update = update)
    
    orgMarts <- names(table(orgBM$mart))
    
    martList <-
        lapply(orgMarts, function(mart)
            dplyr::filter(orgBM, mart == mart))
    
    filtersTXT <-
        paste0("listFilters_", stringr::str_replace(organism, " ", "_"))
    
    if (!file.exists(file.path(tempdir(), "_biomart"))) {
        dir.create(file.path(tempdir(), "_biomart"))
    }
    
    if (!file.exists(file.path(tempdir(), "_biomart", 
                               paste0(filtersTXT, ".txt")))) {
        filtersList <- lapply(martList, function(mart) {
            mart <- as.data.frame(mart)
            
            mart_tbl <-
                do.call(rbind, lapply(1:nrow(mart),
                                      function(dataset) {
                                          filters_tbl <- 
                                    getFilters(dataset = mart$dataset[dataset],
                                                mart    = mart$mart[dataset])
                                          
                                    datasetVec <-
                                  rep(mart$dataset[dataset], nrow(filters_tbl))
                                          
                                    filters_tbl <-
                              dplyr::mutate(filters_tbl, dataset = datasetVec)
                                          
                                          return(filters_tbl)
                                      }))
            
            martVec <-
                rep(mart$mart[1], nrow(mart_tbl))
            mart_tbl <-
                dplyr::mutate(mart_tbl, mart = martVec)
            return(mart_tbl)
        })
        
        utils::write.table(
            do.call(rbind, filtersList),
            file.path(tempdir(), "_biomart", paste0(filtersTXT, ".txt")),
            sep       = "\t",
            quote     = FALSE,
            col.names = TRUE,
            row.names = FALSE
        )
        
    }
    
    filterTable <-
        readr::read_tsv(
            file.path(tempdir(), "_biomart", paste0(filtersTXT, ".txt")),
            col_names = TRUE,
            col_types = readr::cols(
                "name" = readr::col_character(),
                "description" = readr::col_character(),
                "dataset" = readr::col_character(),
                "mart" = readr::col_character()
            )
        )
    
    # summ_filterTable <- dplyr::summarise(dplyr::group_by(filterTable, name), 
    # description = names(table(description)), mart = names(table(mart)), 
    # dataset = names(table(dataset)))
    
    if (!is.null(topic)) {
        findTopic <-
            which(sapply(filterTable$name, function(x)
                stringr::str_detect(x, topic)))
        
        if (dim(filterTable[findTopic , ])[1] == 0)
            stop("Unfortunately the topic '", topic , "' could not be found.")
        
        return(filterTable[findTopic , ])
        
    } else {
        return(filterTable)
    }
}







