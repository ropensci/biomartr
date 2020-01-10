#' @title Helper function for retrieving biological sequence files from 
#' ENSEMBLGENOMES
#' @description This function downloads gff files of query organisms from 
#' ENSEMBLGENOMES
#' @param organism scientific name of the organism of interest.
#' @param release the ENSEMBLGENOMES release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param type biological sequence type.
#' @param id.type id type.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.Seq <-
    function(organism,
             release = NULL,
             type = "dna",
             id.type = "toplevel",
             path) {
        
        if (!is.element(type, c("dna", "cds", "pep", "ncrna")))
            stop("Please a 'type' argument supported by this function: 
                 'dna', 'cds', 'pep', 'ncrna'.")
        
        name <- NULL
        # test if REST API is responding
        is.ensemblgenomes.alive()
        
        if (is.taxid(organism))
            stop("Unfortunately, taxid retrieval is not yet implemented for ENSEMBLGENOMES...", call. = FALSE)

        
        if ( !suppressMessages(is.genome.available(organism = organism, db = "ensemblgenomes", details = FALSE)) ) {
            warning("Unfortunately organism '", organism, "' is not available at ENSEMBLGENOMES. ",
                    "Please check whether or not the organism name is typed correctly or try db = 'ensembl'.",
                    " Thus, download of this species has been omitted. ", call. = FALSE)
            return(FALSE)
        } else {
            
            taxon_id <- assembly <- accession <- NULL
            
            new.organism <- stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))
            
                ensembl_summary <-
                        suppressMessages(is.genome.available(
                                organism = organism,
                                db = "ensemblgenomes",
                                details = TRUE
                        ))
                
                if (nrow(ensembl_summary) == 0) {
                    message("Unfortunately, organism '",organism,"' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted.")
                    return(FALSE)
                }
                
                if (nrow(ensembl_summary) > 1) {
                        
                        if (is.taxid(organism)) {
                                ensembl_summary <-
                                        dplyr::filter(ensembl_summary, taxon_id == as.integer(organism), !is.na(assembly))
                        } else {
                        
                                ensembl_summary <-
                                        dplyr::filter(ensembl_summary,
                                                      (name == stringr::str_to_lower(new.organism)) |
                                                              (accession == organism),
                                                      !is.na(assembly)) }
                        
                        message("Several entries were found for '", organism, "'.")
                        #    "... The first entry '", ensembl_summary$name[1],"' with accession id '",ensembl_summary$accession[1],"' was selected for download.")
                        message("In case you wish to retrieve another genome version please consult is.genome.available(organism = '", organism,"', details = TRUE, db = 'ensemblgenomes') and specify another accession id as organism argument.")
                        message("\n")
                        # select only first entry
                }
            

                new.organism <-
                        paste0(
                                stringr::str_to_upper(stringr::str_sub(ensembl_summary$name[1], 1, 1)),
                                stringr::str_sub(ensembl_summary$name[1], 2, nchar(ensembl_summary$name[1]))
                        )
                
                # retrieve detailed information for organism of interest
        }
        
        get.org.info <- ensembl_summary
        
        rest_url <- paste0(
            "http://rest.ensembl.org/info/assembly/",
            new.organism,
            "?content-type=application/json"
        )
        
        rest_api_status <- test_url_status(url = rest_url, organism = organism)
        
        
        if (is.logical(rest_api_status)) {
            return(FALSE)
        } else {
            
        if (get.org.info$division == "EnsemblBacteria") {
            if (!file.exists(file.path(tempdir(), "EnsemblBacteria.txt"))) {
                tryCatch({
                    custom_download(
"ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt",
                        destfile = file.path(tempdir(), "EnsemblBacteria.txt"),
                        mode = "wb"
                    )
                }, error = function(e) {
                    message(
                        "Something went wrong when accessing the API 'http://rest.ensemblgenomes.org'.",
                        " Are you connected to the internet? ",
                        "Is the homepage 'ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt' ",
                        "currently available? Could it be that the scientific name is mis-spelled or includes special characters such as '.' or '('?"
                    )
                })
            }
            
            suppressWarnings(
                bacteria.info <-
                    readr::read_delim(
                        file.path(tempdir(), "EnsemblBacteria.txt"),
                        delim = "\t",
                        quote = "\"",
                        escape_backslash = FALSE,
                        col_names = c(
                            "name",
                            "species",
                            "division",
                            "taxonomy_id",
                            "assembly",
                            "assembly_accession",
                            "genebuild",
                            "variation",
                            "pan_compara",
                            "peptide_compara",
                            "genome_alignments",
                            "other_alignments",
                            "core_db",
                            "species_id"
                        ),
                        col_types = readr::cols(
                            name = readr::col_character(),
                            species = readr::col_character(),
                            division = readr::col_character(),
                            taxonomy_id = readr::col_integer(),
                            assembly = readr::col_character(),
                            assembly_accession = readr::col_character(),
                            genebuild = readr::col_character(),
                            variation = readr::col_character(),
                            pan_compara = readr::col_character(),
                            peptide_compara = readr::col_character(),
                            genome_alignments = readr::col_character(),
                            other_alignments = readr::col_character(),
                            core_db = readr::col_character(),
                            species_id = readr::col_integer()
                        ),
                        comment = "#"
                    )
            )
            
            # parse for wrong name conventions and fix them...
            organism <-
                stringr::str_replace_all(organism, " sp ", " sp. ")
            organism <-
                stringr::str_replace_all(organism, " pv ", " pv. ")
            organism <-
                stringr::str_replace_all(organism, " str ", " str. ")
            organism <-
                stringr::str_replace_all(organism, " subsp ", " subsp. ")
            organism <-
                stringr::str_replace_all(organism, "\\(", "")
            organism <-
                stringr::str_replace_all(organism, "\\)", "")
            
            assembly <- NULL
            bacteria.info <-
                dplyr::filter(bacteria.info,
                              assembly == get.org.info$assembly)
            
            if (nrow(bacteria.info) == 0) {
                message(
                    "Unfortunately organism '",
                    ensembl_summary$display_name,
                    "' could not be found. Have you tried another database yet? ",
                    "E.g. db = 'ensembl'? Thus, download for this species is omitted."
                )
                return(FALSE)
            }
            
            if (is.na(bacteria.info$core_db[1])) {
                message(
                    "Unfortunately organism '",
                    ensembl_summary$display_name,
                    "' was not assigned to a bacteria collection. 
                    Thus download for this species is omitted."
                )
                return(FALSE)
            }
            
            release_api <- jsonlite::fromJSON(
                    "http://rest.ensembl.org/info/eg_version?content-type=application/json"
            )
            
            if (!is.null(release)){
                    if (!is.element(release, seq_len(as.integer(release_api))))
                            stop("Please provide a release number that is supported by ENSEMBLGENOMES.", call. = FALSE)
            }
            
            # construct retrieval query
            if (is.null(release))
                    core_path <- "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/fasta/"
               
            if (!is.null(release))
                    core_path <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/bacteria/fasta/")

            ensembl.qry <-
                paste0(core_path,
                    paste0(unlist(
                        stringr::str_split(bacteria.info$core_db[1], "_")
                    )[1:3], collapse = "_"),
                    "/",
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
                        ifelse(id.type == "none", "", "."),
                        ifelse(id.type == "none", "", id.type),
                        ".fa.gz"
                    )
                )
            
        } else {
                
                release_api <- jsonlite::fromJSON(
                        "http://rest.ensembl.org/info/eg_version?content-type=application/json"
                )
                
                if (!is.null(release)){
                        if (!is.element(release, seq_len(as.integer(release_api))))
                                stop("Please provide a release number that is supported by ENSEMBLGENOMES.", call. = FALSE)
                }
                
                # construct retrieval query
                if (is.null(release))
                        core_path <- "ftp://ftp.ensemblgenomes.org/pub/current/"
                
                if (!is.null(release))
                        core_path <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/")
                
            # construct retrieval query
            ensembl.qry <-
                paste0( core_path,
                    stringr::str_to_lower(
                        stringr::str_replace(get.org.info$division[1], 
                                             "Ensembl", "")
                    ),
                    "/fasta/",
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
                        ifelse(id.type == "none", "", "."),
                        ifelse(id.type == "none", "", id.type),
                        ".fa.gz"
                    )
                )
        }
        
        # if (!exists.ftp.file(url = ensembl.qry, file.path = ensembl.qry)) {
        #     message(
        #         "Unfortunately no ",
        #         type,
        #         " file could be found for organism '",
        #         organism,
        #         "'. Thus, the download of this organism has been omitted."
        #     )
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
                ifelse(id.type == "none", "", "."),
                ifelse(id.type == "none", "", id.type),
                ".fa.gz"
            )
        ))) {
            message(
                "File ",
                file.path(
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
                " exists already. Thus, download has been skipped."
            )
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
                                ))
        }
        
        return(c(file.path(
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
        ), ensembl.qry))
        
        }
    }
