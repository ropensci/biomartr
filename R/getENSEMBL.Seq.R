#' @title Helper function for retrieving biological sequence files from ENSEMBL
#' @description This function downloads gff files of query 
#' organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type biological sequence type.
#' @param id.type id type.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd


getENSEMBL.Seq <- function(organism, type = "dna", id.type = "toplevel", path) {
    
    if (!is.element(type, c("dna", "cds", "pep", "ncrna")))
        stop("Please a 'type' argument supported by this function: 
             'dna', 'cds', 'pep', 'ncrna'.")
    
    
    ensembl_summary <-
        suppressMessages(is.genome.available(
            organism = organism,
            db = "ensembl",
            details = TRUE
        ))
    
    if (nrow(ensembl_summary) == 0) {
        message("Unfortunately, organism '",organism,"' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted.")
        return(FALSE)
    }
    
    taxon_id <- assembly <- name <- accession <- NULL
    
    if (nrow(ensembl_summary) > 1) {
        if (is.taxid(organism)) {
            ensembl_summary <-
                dplyr::filter(ensembl_summary, taxon_id == as.integer(organism), !is.na(assembly))
        } else {
            
            ensembl_summary <-
                dplyr::filter(
                    ensembl_summary,
                    (name == stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))) |
                        (accession == organism),
                        !is.na(assembly)
                )
        }
    }
    
    new.organism <- ensembl_summary$name[1]
    new.organism <-
        paste0(
            stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
            stringr::str_sub(new.organism, 2, nchar(new.organism))
        )
    
    
    rest_url <- paste0(
        "http://rest.ensembl.org/info/assembly/",
        new.organism,
        "?content-type=application/json"
    )
    
    rest_api_status <- test_url_status(url = rest_url, organism = organism)   
    if (is.logical(rest_api_status)) {
        return(FALSE)
    } else {
        
        # construct retrieval query
        ensembl.qry <-
            paste0(
                "ftp://ftp.ensembl.org/pub/current_fasta/",
                stringr::str_to_lower(new.organism),
                "/",
                type,
                "/",
                paste0(
                    new.organism,
                    ".",
                    rest_api_status$default_coord_system_version,
                    ".",
                    type,
                    ifelse(id.type == "none","","."),
                    ifelse(id.type == "none","",id.type),
                    ".fa.gz"
                )
            )
        
        # if (!exists.ftp.file(url = ensembl.qry, file.path = ensembl.qry)) {
        #     message("Unfortunately no ",type," file could be found for organism '",
        #             organism,
        #             "'. Thus, the download of this organism has been omitted.")
        #     return(FALSE)
        # }

        if (file.exists(file.path(
            path,
            paste0(
                new.organism,
                ".",
                rest_api_status$default_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none","","."),
                ifelse(id.type == "none","",id.type),
                ".fa.gz"
            )
        ))) {
            message("File ",file.path(
                path,
                paste0(
                    new.organism,
                    ".",
                    rest_api_status$default_coord_system_version,
                    ".",
                    type,
                    ifelse(id.type == "none","","."),
                    ifelse(id.type == "none","",id.type),
                    ".fa.gz"
                )
            )," exists already. Thus, download has been skipped.")
            return(file.path(
                path,
                paste0(
                    new.organism,
                    ".",
                    rest_api_status$default_coord_system_version,
                    ".",
                    type,
                    ifelse(id.type == "none","","."),
                    ifelse(id.type == "none","",id.type),
                    ".fa.gz"
                )
            ))
        } else {
                custom_download(url = ensembl.qry,
                                destfile = file.path(
                                    path,
                                    paste0(
                                        new.organism,
                                        ".",
                                        rest_api_status$default_coord_system_version,
                                        ".",
                                        type,
                                        ifelse(id.type == "none", "", "."),
                                        ifelse(id.type == "none", "", id.type),
                                        ".fa.gz"
                                    )
                                ),
                                mode = "wb")
                
                return(file.path(
                    path,
                    paste0(
                        new.organism,
                        ".",
                        rest_api_status$default_coord_system_version,
                        ".",
                        type,
                        ifelse(id.type == "none", "", "."),
                        ifelse(id.type == "none", "", id.type),
                        ".fa.gz"
                    )
                ))
        }
    }
}
