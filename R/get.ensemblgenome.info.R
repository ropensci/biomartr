#' @title Helper function to retrieve species information 
#' from the ENSEMBLGENOMES API
#' @description This function interfaces with the ENSEMBL API 
#' (http://rest.ensemblgenomes.org/info/species?content-type=application/json)
#' and internally stores the output to use this information for subsequent
#' retrieval function calls.
#' @author Hajk-Georg Drost
#' @noRd
get.ensemblgenome.info <- function(update = FALSE) {
    if (file.exists(file.path(tempdir(), "ensemblgenome_info.tsv")) &&
        !update) {
        suppressWarnings(
            ensemblgenome.info <-
                readr::read_tsv(
                    file.path(tempdir(), "ensemblgenome_info.tsv"),
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
        
        message("Starting retrieval of information for all species stored in ENSEMBLGENOMES... This needs to be done only once.")
        
        rest_url <- "http://rest.ensembl.org/info/species?content-type=application/json"
        rest_api_status <- curl::curl_fetch_memory(rest_url)
        
        if (rest_api_status$status_code != 200) {
            message(
                "The API 'rest.ensembl.org' does not 
                seem to respond or work properly.
                Is the homepage 'rest.ensembl.org' 
                currently available?", 
                " Could it be that there is a firewall issue on your side? Please re-run the function and check if it works now."
            )
        }
        
            ensemblgenome.info <-
                tibble::as_tibble(
                    jsonlite::fromJSON(
                        rest_url
                    )$species
                )
        
        aliases <- groups <- NULL
        ensemblgenome.info <-
            dplyr::select(ensemblgenome.info, -aliases, -groups)
        
        readr::write_tsv(ensemblgenome.info,
                         file.path(tempdir(), "ensemblgenome_info.tsv"))
    }
    
    return(ensemblgenome.info)
}
