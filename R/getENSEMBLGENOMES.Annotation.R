getENSEMBLGENOMES.Annotation <- function(organism, type = "dna", id.type = "toplevel", path) {
    
    if (!is.element(type, c("dna", "cds", "pep")))
        stop("Please a 'type' argument supported by this function: 'dna', 'cds', 'pep'.")
    
    # test if REST API is responding
    is.ensemblgenomes.alive()
    
    name <- NULL
    
    new.organism <- stringr::str_replace_all(organism, " ", "_")
    
    if (file.exists(file.path(tempdir(), "ensemblgenomes_summary.txt"))) {
        suppressWarnings(
            ensembl.available.organisms <-
                readr::read_tsv(
                    file.path(tempdir(), "ensemblgenomes_summary.txt"),
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
                )
        )
    }
    
    if (!file.exists(file.path(tempdir(), "ensemblgenomes_summary.txt"))) {
    # check if organism is available on ENSEMBL
    tryCatch({
        ensembl.available.organisms <-
            jsonlite::fromJSON("http://rest.ensemblgenomes.org/info/species?content-type=application/json")
    }, error = function(e)
        stop(
            "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
        ))
        
        aliases <- groups <- NULL
        
        # transform list object returned by 'fromJSON' to tibble
        ensembl.available.organisms <-
            tibble::as_tibble(dplyr::select(ensembl.available.organisms$species, -aliases, -groups))
        
        readr::write_tsv(
            ensembl.available.organisms,
            file.path(tempdir(), "ensemblgenomes_summary.txt")
        )
    }
    
    if (!any(stringr::str_detect(stringr::str_to_lower(new.organism),
                    ensembl.available.organisms$name))) {
            warning(
                "Unfortunately organism '",
                organism,
                "' is not available at ENSEMBL. Please check whether or not the organism name is typed correctly. Thus, download of this species has been omitted."
            )
            return(FALSE)
    }
    
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
            "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
        ))
    
    # retrieve detailed information for organism of interest
    get.org.info <- is.genome.available(organism = organism, details = TRUE, db = "ensemblgenomes")
    
    # retrieve the Ensembl Genomes version of the databases backing this service
    tryCatch({
        eg_version <-
            jsonlite::fromJSON(
                "http://rest.ensemblgenomes.org/info/eg_version?content-type=application/json"
            )
    }, error = function(e)
        stop(
            "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?",
            call. = FALSE
        ))
    
    if (get.org.info$division == "EnsemblBacteria") {
        
        if (!file.exists(file.path(tempdir(),"EnsemblBacteria.txt"))) {
            tryCatch({
                downloader::download(
                    "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt",
                    destfile = file.path(tempdir(),"EnsemblBacteria.txt"),
                    mode = "wb"
                )
            }, error = function(e)
                stop(
                    "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt' currently available?",
                    call. = FALSE
                ))
        }
        
        suppressWarnings(bacteria.info <-
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
                             ))
        
        # parse for wrong name conventions and fix them... 
        organism <- stringr::str_replace_all(organism, " sp ", " sp. ")
        organism <- stringr::str_replace_all(organism, " pv ", " pv. ")
        organism <- stringr::str_replace_all(organism, " str ", " str. ")
        organism <- stringr::str_replace_all(organism, " subsp ", " subsp. ")
        organism <- stringr::str_replace_all(organism,"\\(","")
        organism <- stringr::str_replace_all(organism,"\\)","")
        
        bacteria.info <- dplyr::filter(bacteria.info, stringr::str_detect(name, stringr::coll(organism, ignore_case = TRUE)))
        
        if (nrow(bacteria.info) == 0) {
            warning("Unfortunately organism '",organism,"' could not be found. Thus download for this species is omitted.", call. = FALSE)
            return(FALSE)
        }
            
        
        if (is.na(bacteria.info$core_db)) {
            warning("Unfortunately organism '",organism,"' was not assigned to a bacteria collection. Thus download for this species is omitted.", call. = FALSE)
            return(FALSE)
        }
        
        # construct retrieval query
        ensembl.qry <-
            paste0(
                "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gff3/",
                paste0(unlist(stringr::str_split(bacteria.info$core_db,"_"))[1:3], collapse = "_"),
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
        
        server.folder.path <- paste0(
            "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gff3/",
            paste0(unlist(
                stringr::str_split(bacteria.info$core_db, "_")
            )[1:3], collapse = "_"),
            "/",
            stringr::str_to_lower(new.organism),
            "/"
        )
        tryCatch({get.files <- RCurl::getURL(
            server.folder.path,
            verbose = FALSE,
            ftp.use.epsv = TRUE,
            dirlistonly = TRUE
        )}, error = function(e)
            stop(
                "The server path '",server.folder.path,"' seems not to exist. Please make sure that the selected bacteria is available at ENSEMBLGENOMES.",
                call. = FALSE
            ))
        Sys.sleep(0.33)
        
        if (stringr::str_detect(get.files,"abinitio")) {
            ensembl.qry <-
                paste0(
                    "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/gff3/",
                    paste0(unlist(stringr::str_split(bacteria.info$core_db,"_"))[1:3], collapse = "_"),
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
        # construct retrieval query
        ensembl.qry <-
            paste0(
                "ftp://ftp.ensemblgenomes.org/pub/current/",
                stringr::str_to_lower(stringr::str_replace(get.org.info$division,"Ensembl","")),
                "/gff3/",
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
        
        if (stringr::str_detect(get.files,"abinitio")) {
            ensembl.qry <-
                paste0(
                    "ftp://ftp.ensemblgenomes.org/pub/current/",
                    stringr::str_to_lower(stringr::str_replace(get.org.info$division,"Ensembl","")),
                    "/gff3/",
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
    }
    
    
    tryCatch({
        downloader::download(ensembl.qry,
                             destfile = file.path(
                                 path,
                                 paste0(
                                     new.organism,
                                     ".",
                                     json.qry.info$default_coord_system_version,
                                     ".",
                                     eg_version,
                                     "_ensemblgenomes",
                                     ".gff3.gz"
                                 )
                             ),
                             mode = "wb")
    }, error = function(e)
        stop(
            "The FTP site of ENSEMBLGENOMES 'ftp://ftp.ensemblgenomes.org/current/gff3' does not seem to work properly. Are you connected to the internet? Is the site 'ftp://ftp.ensemblgenomes.org/current/gff3' or 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
        ))
    
    return(file.path(
        path,
        paste0(
            new.organism,
            ".",
            json.qry.info$default_coord_system_version,
            ".",
            eg_version,
            "_ensemblgenomes",
            ".gff3.gz"
        )
    ))
}
