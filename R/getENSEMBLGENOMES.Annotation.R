#' @title Annotation file retrieval from ENSEMBLGENOMES
#' @description Helper function for retrieving GFF files
#' of a particular organism of interest from the ENSEMBLGENOMES ftp server
#' @param organism scientific name of the query organism
#' @param type type of biological sequence
#' @param id.type ENSEMBLGENOMES id type
#' @param release the ENSEMBLGENOMES release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param path where shall file be saved?
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.Annotation <-
    function(organism,
             type = "dna",
             id.type = "toplevel",
             release = NULL,
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
                        "http://rest.ensembl.org/info/assembly/",
                        new.organism,
                        "?content-type=application/json"
                    )
                )
        }, error = function(e)
            message(
                "The API 'http://rest.ensembl.org' does not seem to respond or work properly. Do you have a stable internet connection or could there be an issue with your firewall?"
            ))
        
        # retrieve detailed information for organism of interest
        get.org.info <- ensemblgenomes_summary
        
        # retrieve the Ensembl Genomes version of the 
        # databases backing this service
        tryCatch({
            eg_version <-
                jsonlite::fromJSON(
 "http://rest.ensembl.org/info/eg_version?content-type=application/json"
                )
        }, error = function(e)
            message(
                "The API 'http://rest.ensembl.org' does not seem to work properly. Do you have a stable internet connection or could there be an issue with your firewall?"
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
                    message(
                        "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Do you have a stable internet connection?"
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
                    "' was not assigned to a bacteria collection. 
                    Thus download for this species is omitted.",
                    call. = FALSE
                )
                return(FALSE)
            }
            
            release_api <- jsonlite::fromJSON(
                    "http://rest.ensembl.org/info/eg_version?content-type=application/json"
            )
            
            if (!is.null(release)){
                    if (!is.element(release, seq_len(as.integer(release_api))))
                            stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
            }
            
            # construct retrieval query
            if (is.null(release))
                    core_path <- "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gff3/"
            
            if (!is.null(release))
                    core_path <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/bacteria/gff3/")
            
            # construct retrieval query
            ensembl.qry <-
                paste0(
                        core_path,
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
                        ".gff3.gz"
                    )
                )
            
            if (is.null(release)) {
                    server.folder.path <- paste0(
                            "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gff3/",
                            paste0(unlist(
                                    stringr::str_split(bacteria.info$core_db[1], "_")
                            )[1:3], collapse = "_"),
                            "/",
                            stringr::str_to_lower(new.organism),
                            "/"
                    )
            }
            
            if (!is.null(release)) {
                    server.folder.path <- paste0(
                            "ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/bacteria/gff3/",
                            paste0(unlist(
                                    stringr::str_split(bacteria.info$core_db[1], "_")
                            )[1:3], collapse = "_"),
                            "/",
                            stringr::str_to_lower(new.organism),
                            "/"
                    )
            }
            
            tryCatch({
                get.files <- RCurl::getURL(
                    server.folder.path,
                    verbose = FALSE,
                    ftp.use.epsv = TRUE,
                    dirlistonly = TRUE
                )
            }, error = function(e)
                message(
                    "The server path '",
                    server.folder.path,
                    "' seems not to exist. Please make sure that the selected 
                    bacteria is available at ENSEMBLGENOMES under the specified release."
                ))
            
            if (stringr::str_detect(get.files, "abinitio")) {
                    
                    if (is.null(release))
                            core_path2 <- "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gff3/"
                    if (!is.null(release))
                            core_path2 <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release,"/bacteria/gff3/")
                    
                ensembl.qry <-
                    paste0(
                            core_path2,
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
                            ".abinitio.gff3.gz"
                        )
                    )
            }
            
        } else {
                release_api <- jsonlite::fromJSON(
                        "http://rest.ensembl.org/info/eg_version?content-type=application/json"
                )
                
                if (!is.null(release)){
                        if (!is.element(release, seq_len(as.integer(release_api))))
                                stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
                }
                
                # construct retrieval query
                if (is.null(release))
                        core_path <- "ftp://ftp.ensemblgenomes.org/pub/current/"
                
                if (!is.null(release))
                        core_path <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/")
                
            # construct retrieval query
            ensembl.qry <-
                paste0(
                        core_path,
                    stringr::str_to_lower(
                        stringr::str_replace(get.org.info$division[1], 
                                             "Ensembl", "")
                    ),
                    "/gff3/",
                    stringr::str_to_lower(new.organism),
                    "/",
                    paste0(
                        new.organism,
                        ".",
                        json.qry.info$default_coord_system_version,
                        ".",
                        ifelse(is.null(release), eg_version, release),
                        ".gff3.gz"
                    )
                )
            
            if (file.exists(file.path(
                path,
                paste0(
                    new.organism,
                    ".",
                    json.qry.info$default_coord_system_version,
                    ".",
                    ifelse(is.null(release), eg_version, release),
                    "_ensemblgenomes",
                    ".gff3.gz"
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
                            ifelse(is.null(release), eg_version, release),
                            "_ensemblgenomes",
                            ".gff3.gz"
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
                                   ifelse(is.null(release), eg_version, release),
                                            "_ensemblgenomes",
                                            ".gff3.gz"
                                        )
                                    ),
                                    mode = "wb")
                }, error = function(e) {
                        message(
                                "Something went wrong when trying to retrieve file ",
                                ensembl.qry,
                                " from ENSEMBLGENOMES. Could it be that the species ",
                                organism,
                                " does not have an entry for your specified release version?"
                        )
                })
            }
        }
        
        return(c(file.path(
            path,
            paste0(
                new.organism,
                ".",
                json.qry.info$default_coord_system_version,
                ".",
                ifelse(is.null(release), eg_version, release),
                "_ensemblgenomes",
                ".gff3.gz"
            )
        ), ensembl.qry))
}
