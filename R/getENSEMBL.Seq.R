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
    
    new.organism <- stringr::str_replace_all(organism, " ", "_")
    
    if ( !suppressMessages(is.genome.available(organism = organism, db = "ensembl", details = FALSE)) ) {
        stop("Unfortunately organism '", organism, "' is not available at ENSEMBL. ",
                "Please check whether or not the organism name is typed correctly or try db = 'ensemblgenomes'.",
                " Thus, download of this species has been omitted. ", call. = FALSE)
    } else {
        ensembl_summary <- suppressMessages(is.genome.available(organism = organism, db = "ensembl", details = TRUE))
        new.organism <- paste0(stringr::str_to_upper(stringr::str_sub(ensembl_summary$name, 1, 1)), stringr::str_sub(ensembl_summary$name, 2, nchar(ensembl_summary$name)))
    }
    
    # test proper API access
    tryCatch({
        json.qry.info <-
            jsonlite::fromJSON(
                paste0(
                    "http://rest.ensembl.org/info/assembly/",
                    new.organism,
                    "?content-type=application/json"
                )
            )
    }, error = function(e) {
        warning(
            "The API 'http://rest.ensembl.org' does not seem to work properly. ", 
            "Are you connected to the internet? Is the homepage ",
            "'http://rest.ensembl.org' currently available?", call. = FALSE
        )
        return(FALSE)
    })
    
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
                json.qry.info$default_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none","","."),
                ifelse(id.type == "none","",id.type),
                ".fa.gz"
            )
        )
    
    if (!exists.ftp.file(url = ensembl.qry, file.path = ensembl.qry)) {
        message("Unfortunately no ",type," file could be found for organism '",
                organism,
                "'. Thus, the download of this organism has been omitted.")
        return(FALSE) 
    }
    
    if (file.exists(file.path(
            path,
            paste0(
                    new.organism,
                    ".",
                    json.qry.info$default_coord_system_version,
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
                            json.qry.info$default_coord_system_version,
                            ".",
                            type,
                            ifelse(id.type == "none","","."),
                            ifelse(id.type == "none","",id.type),
                            ".fa.gz"
                    )
            )," exists already. Thus, download has been skipped.")
    } else {
        tryCatch({
            custom_download(ensembl.qry,
                            destfile = file.path(
                                path,
                                paste0(
                                    new.organism,
                                    ".",
                                    json.qry.info$default_coord_system_version,
                                    ".",
                                    type,
                                    ifelse(id.type == "none", "", "."),
                                    ifelse(id.type == "none", "", id.type),
                                    ".fa.gz"
                                )
                            ),
                            mode = "wb")
        }, error = function(e) {
            warning(
                "The FTP site of ENSEMBL 'ftp://ftp.ensembl.org/pub/' does not ",
                "seem to work properly. Are you connected to the internet? ",
                "Is the site 'ftp://ftp.ensembl.org/pub/' or ",
                "'http://rest.ensembl.org' currently available?", call. = FALSE
            )
            return(FALSE)
        })
    }
    
    return(file.path(
        path,
        paste0(
            new.organism,
            ".",
            json.qry.info$default_coord_system_version,
            ".",
            type,
            ifelse(id.type == "none", "", "."),
            ifelse(id.type == "none", "", id.type),
            ".fa.gz"
        )
    ))
}
