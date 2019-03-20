#' @title Helper function to retrieve species information from the ENSEMBL API
#' @description This function interfaces with the ENSEMBL API
#' (http://rest.ensembl.org/info/species?content-type=application/json)
#' and internally stores the output to use this information for subsequent
#' retrieval function calls.
#' @author Hajk-Georg Drost
#' @noRd
get.ensembl.info <- function(update = FALSE) {
    if (file.exists(file.path(tempdir(), "ensembl_info.tsv")) &&
        !update) {
        suppressWarnings(
            ensembl.info <-
                readr::read_tsv(
                    file.path(tempdir(), "ensembl_info.tsv"),
                    col_names = TRUE,
                    col_types = readr::cols(
                        division = readr::col_character(),
                        taxon_id = readr::col_integer(),
                        name = readr::col_character(),
                        release = readr::col_integer(),
                        display_name = readr::col_character(),
                        accession = readr::col_character(),
                        common_name = readr::col_character(),
                        assembly = readr::col_character()
                    )
                )
        )
        
    } else {
            
        
        rest_url <- "http://rest.ensembl.org/info/species?content-type=application/json"
        rest_api_status <- curl::curl_fetch_memory(rest_url)
        if (rest_api_status$status_code != 200) {
            stop(
                "The API 'http://rest.ensembl.org' does not seem to
                work properly. Are you connected to the internet?
                Is the homepage 'http://rest.ensembl.org' currently available?",
                call. = FALSE
            )
        }
    
            ensembl.info <-
                tibble::as_tibble(
                    jsonlite::fromJSON(
                        rest_url
                    )$species
                )
        
        aliases <- groups <- NULL
        ensembl.info <-
            dplyr::select(ensembl.info, -aliases, -groups)
        
        readr::write_tsv(ensembl.info,
                         file.path(tempdir(), "ensembl_info.tsv"))
    }
    
    return(ensembl.info)
}
