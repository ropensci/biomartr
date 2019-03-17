#' @title Annotation file retrieval from ENSEMBLGENOMES
#' @description Helper function for retrieving GFF files
#' of a particular organism of interest from the ENSEMBLGENOMES ftp server
#' @param organism scientific name of the query organism
#' @param type type of biological sequence
#' @param id.type ENSEMBLGENOMES id type
#' @param path where shall file be saved?
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.gtf <-
    function(organism,
             type = "dna",
             id.type = "toplevel",
             path) {
        
        if (!is.element(type, c("dna", "cds", "pep")))
            stop("Please a 'type' argument supported by this function: 
                 'dna', 'cds', 'pep'.")
        
            ensemblgenomes_summary <-
                    suppressMessages(is.genome.available(
                            organism = organism,
                            db = "ensemblgenomes",
                            details = TRUE
                    ))
            
            if (nrow(ensemblgenomes_summary) == 0) {
                    message("Unfortunately, organism '",organism,"' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted.")
                    return(FALSE)
            }
            
            taxon_id <- assembly <- name <- accession <- NULL
            
            if (nrow(ensemblgenomes_summary) > 1) {
                    if (is.taxid(organism)) {
                            ensemblgenomes_summary <-
                                    dplyr::filter(ensemblgenomes_summary, taxon_id == as.integer(organism), !is.na(assembly))
                    } else {
                            
                            ensemblgenomes_summary <-
                                    dplyr::filter(
                                            ensemblgenomes_summary,
                                            (name == stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))) |
                                                    (accession == organism),
                                            !is.na(assembly)
                                    )
                    }
            }
            
            new.organism <- ensemblgenomes_summary$name[1]
            new.organism <-
                    paste0(
                            stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
                            stringr::str_sub(new.organism, 2, nchar(new.organism))
                    )
            
            
        # test proper API access
        tryCatch({
            json.qry.info <-
                jsonlite::fromJSON(
                    paste0(
                        "http://rest.ensemblgenomes.org/info/assembly/",
                        new.organism,
                        "?content-type=application/json"
                    )
                )
        }, error = function(e)
            stop(
                "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Do you have a stable internet connection?",
                call. = FALSE
            ))
        
        # retrieve detailed information for organism of interest
        get.org.info <- ensemblgenomes_summary
        
        # retrieve the Ensembl Genomes version of the 
        # databases backing this service
        tryCatch({
            eg_version <-
                jsonlite::fromJSON(
                    "http://rest.ensemblgenomes.org/info/eg_version?content-type=application/json"
                )
        }, error = function(e)
            stop(
                "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Do you have a stable internet connection?",
                call. = FALSE
            ))
        
        if (get.org.info$division == "EnsemblBacteria") {
            if (!file.exists(file.path(tempdir(), "EnsemblBacteria.txt"))) {
                tryCatch({
                    custom_download(
                        "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt",
                        destfile = file.path(tempdir(), "EnsemblBacteria.txt"),
                        mode = "wb"
                    )
                }, error = function(e)
                    stop(
                        "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Do you have a stable internet connection?",
                        call. = FALSE
                    ))
            }
            
            suppressWarnings(
                bacteria.info <-
                    readr::read_tsv(
                        file.path(tempdir(), "EnsemblBacteria.txt"),
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
            
            bacteria.info <-
                dplyr::filter(bacteria.info,
                              stringr::str_detect(name, stringr::coll(organism, 
                                                                      ignore_case = TRUE)))
            
            if (nrow(bacteria.info) == 0) {
                warning(
                    "Unfortunately organism '",
                    organism,
                    "' could not be found. Thus download for this 
                    species is omitted.",
                    call. = FALSE
                )
                return(FALSE)
            }
            
            
            if (is.na(bacteria.info$core_db[1])) {
                warning(
                    "Unfortunately organism '",
                    organism,
                    "' was not assigned to a bacteria collection. Thus download for this species is omitted.",
                    call. = FALSE
                )
                return(FALSE)
            }
            
            # construct retrieval query
            ensembl.qry <-
                paste0(
                    "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gtf/",
                    paste0(unlist(
                        stringr::str_split(bacteria.info$core_db[1], "_")
                    )[1:3], collapse = "_"),
                    "/",
                    stringr::str_to_lower(new.organism),
                    "/",
                    paste0(
                        new.organism,
                        ".",
                        json.qry.info$default_coord_system_version,
                        ".",
                        eg_version,
                        ".gtf.gz"
                    )
                )
            
            server.folder.path <- paste0(
                "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gtf/",
                paste0(unlist(
                    stringr::str_split(bacteria.info$core_db[1], "_")
                )[1:3], collapse = "_"),
                "/",
                stringr::str_to_lower(new.organism),
                "/"
            )
            tryCatch({
                get.files <- RCurl::getURL(
                    server.folder.path,
                    verbose = FALSE,
                    ftp.use.epsv = TRUE,
                    dirlistonly = TRUE
                )
            }, error = function(e)
                stop(
                    "The server path '",
                    server.folder.path,
                    "' seems not to exist. Please make sure that the selected bacteria is available at ENSEMBLGENOMES.",
                    call. = FALSE
                ))
            
            if (stringr::str_detect(get.files, "abinitio")) {
                ensembl.qry <-
                    paste0(
                        "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gtf/",
                        paste0(unlist(
                            stringr::str_split(bacteria.info$core_db[1], "_")
                        )[1:3], collapse = "_"),
                        "/",
                        stringr::str_to_lower(new.organism),
                        "/",
                        paste0(
                            new.organism,
                            ".",
                            json.qry.info$default_coord_system_version,
                            ".",
                            eg_version,
                            ".abinitio.gtf.gz"
                        )
                    )
            }
            
        } else {
            # construct retrieval query
            ensembl.qry <-
                paste0(
                    "ftp://ftp.ensemblgenomes.org/pub/current/",
                    stringr::str_to_lower(
                        stringr::str_replace(get.org.info$division[1], 
                                             "Ensembl", "")
                    ),
                    "/gtf/",
                    stringr::str_to_lower(new.organism),
                    "/",
                    paste0(
                        new.organism,
                        ".",
                        json.qry.info$default_coord_system_version,
                        ".",
                        eg_version,
                        ".gtf.gz"
                    )
                )
            
            if (file.exists(file.path(
                path,
                paste0(
                    new.organism,
                    ".",
                    json.qry.info$default_coord_system_version,
                    ".",
                    eg_version,
                    "_ensemblgenomes",
                    ".gtf.gz"
                )
            ))) {
                message(
                    "File ",
                    file.path(
                        path,
                        paste0(
                            new.organism,
                            ".",
                            json.qry.info$default_coord_system_version,
                            ".",
                            eg_version,
                            "_ensemblgenomes",
                            ".gtf.gz"
                        )
                    ),
                    " exists already. Thus, download has been skipped."
                )
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
                                            eg_version,
                                            "_ensemblgenomes",
                                            ".gtf.gz"
                                        )
                                    ),
                                    mode = "wb")
                }, error = function(e)
                    stop(
                        "Something went wrong while trying to reach the file '",ensembl.qry,"'. This could be due to an instable internet connection or incorrect file path on the ENSEMBLGENOMES ftp server. Please check if you are able to reach '",ensembl.qry, "' in your web browser.",
                        " In some cases ENSEMBLGENOME released a new database version and path names or the API weren't updated yet. Please give it a few days time or contact helpdesk@ensemblgenomes.org.",
                        call. = FALSE
                    ))
            }
        }
        
        return(file.path(
            path,
            paste0(
                new.organism,
                ".",
                json.qry.info$default_coord_system_version,
                ".",
                eg_version,
                "_ensemblgenomes",
                ".gtf.gz"
            )
        ))
        }
