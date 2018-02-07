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
    
    # test if REST API is responding
    is.ensembl.alive()
    if (file.exists(file.path(tempdir(), "ensembl_summary.txt"))) {
        suppressWarnings(ensembl.available.organisms <-
                             readr::read_delim(
                                 file.path(tempdir(), "ensembl_summary.txt"),
                                 delim = "\t",
                                 quote = "\"",
                                 escape_backslash = FALSE,
                                 col_names = c(
                                     "division",
                                     "taxon_id",
                                     "name",
                                     "release",
                                     "display_name",
                                     "accession",
                                     "common_name",
                                     "assembly"
                                 ),
                                 col_types = readr::cols(
                                     division = readr::col_character(),
                                     taxon_id = readr::col_integer(),
                                     name = readr::col_character(),
                                     release = readr::col_integer(),
                                     display_name = readr::col_character(),
                                     accession = readr::col_character(),
                                     common_name = readr::col_character(),
                                     assembly = readr::col_character()
                                 ),
                                 comment = "#"
                             ))
    }
    
    if (!file.exists(file.path(tempdir(), "ensembl_summary.txt"))) {
        # check if organism is available on ENSEMBL
        tryCatch({
            ensembl.available.organisms <-
                jsonlite::fromJSON(
           "http://rest.ensembl.org/info/species?content-type=application/json")
        }, error = function(e) {
            warning(
                "The API 'http://rest.ensembl.org' does not seem to work properly.",
                " Are you connected to the internet? Is the homepage ",
                "'http://rest.ensembl.org' currently available? ",
                "Do you have a fast and stable internet connection?", 
                call. = FALSE
            )
            return(FALSE)
        }
            )
        
        aliases <- groups <- NULL
        
        # transform list object returned by 'fromJSON' to tibble
        ensembl.available.organisms <-
            tibble::as_tibble(dplyr::select(ensembl.available.organisms$species,
                                            -aliases, -groups))
        
        readr::write_tsv(ensembl.available.organisms,
                         file.path(tempdir(), "ensembl_summary.txt"))
    }
    
    if (!is.element(stringr::str_to_lower(new.organism),
                    ensembl.available.organisms$name)) {
        
        warning(
            "Unfortunately organism '",
            organism,
            "' is not available at ENSEMBL. Please check whether or not ",
            "the organism name is typed correctly or try db = 'ensemblgenomes'. Thus, download of this ",
            "species has been omitted.", call. = FALSE
        )
        return(FALSE)
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
    }
        )
    
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
