getENSEMBLGENOMES.Seq <- function(organism, type = "dna", id.type = "toplevel", path) {
    
    if (!is.element(type, c("dna", "cds", "pep", "ncrna")))
        stop("Please a 'type' argument supported by this function: 'dna', 'cds', 'pep', 'ncrna'.")
    
    new.organism <- stringr::str_replace_all(organism, " ", "_")
    name <- NULL
    # test if REST API is responding
    is.ensemblgenomes.alive()
    
    if (file.exists(file.path(tempdir(), "ensemblgenomes_summary.txt"))) {
        suppressWarnings(
            ensembl.available.organisms <-
                readr::read_delim(
                    file.path(tempdir(), "ensemblgenomes_summary.txt"),
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
                )
        )
    }
    
    if (!file.exists(file.path(tempdir(), "ensemblgenomes_summary.txt"))) {
        # check if organism is available on ENSEMBL
        tryCatch({
            ensembl.available.organisms <-
                jsonlite::fromJSON(
                    "http://rest.ensemblgenomes.org/info/species?content-type=application/json"
                )
        }, error = function(e) {
            warning(
                "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
            )
            return(FALSE)
        }
            )
        
        aliases <- groups <- NULL
        
        # transform list object returned by 'fromJSON' to tibble
        ensembl.available.organisms <-
            tibble::as_tibble(dplyr::select(ensembl.available.organisms$species, -aliases, -groups))
        
        readr::write_tsv(
            ensembl.available.organisms,
            file.path(tempdir(), "ensemblgenomes_summary.txt")
        )
    }
    
    if (!is.element(stringr::str_to_lower(new.organism),
                    ensembl.available.organisms$name)) {
        
        warning(
            "Unfortunately organism '",
            organism,
            "' is not available at ENSEMBLGENOMES. Please check whether or not the organism name is typed correctly. Thus, download of this species has been omitted."
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
    }, error = function(e) {
        warning(
            "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?"
        )
        return(FALSE)
    }
        )
    
    # retrieve detailed information for organism of interest
    get.org.info <- is.genome.available(organism = organism, details = TRUE, db = "ensemblgenomes")
    
    if (get.org.info$division == "EnsemblBacteria") {
        
        if (!file.exists(file.path(tempdir(),"EnsemblBacteria.txt"))) {
            tryCatch({
                downloader::download(
                    "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt",
                    destfile = file.path(tempdir(),"EnsemblBacteria.txt"),
                    mode = "wb"
                )
            }, error = function(e) {
                warning(
                    "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt' currently available?",
                    call. = FALSE
                )
                return(FALSE)
            }
                )
        }
        
        suppressWarnings(bacteria.info <-
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
        
        
        if (is.na(bacteria.info$core_db[1])) {
            warning("Unfortunately organism '",organism,"' was not assigned to a bacteria collection. Thus download for this species is omitted.", call. = FALSE)
            return(FALSE)
        }
        
        # construct retrieval query
        ensembl.qry <-
            paste0(
                "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/fasta/",
                paste0(unlist(stringr::str_split(bacteria.info$core_db[1],"_"))[1:3], collapse = "_"),
                "/",
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
        
    } else {
        # construct retrieval query
        ensembl.qry <-
            paste0(
                "ftp://ftp.ensemblgenomes.org/pub/current/",
                stringr::str_to_lower(stringr::str_replace(get.org.info$division[1],"Ensembl","")),
                "/fasta/",
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
    }
    
    if (!exists.ftp.file(url = paste0(
        "ftp://ftp.ensemblgenomes.org/pub/current/",
        stringr::str_to_lower(stringr::str_replace(get.org.info$division[1],"Ensembl","")),
        "/fasta/",
        stringr::str_to_lower(new.organism),
        "/",
        type,
        "/"), file.path = ensembl.qry)) {
        message("Unfortunately no ",type," file could be found for organism '",organism,"'. Thus, the download of this organism has been omitted.")
        return(FALSE) 
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
                                     type,
                                     ifelse(id.type == "none","","."),
                                     ifelse(id.type == "none","",id.type),
                                     ".fa.gz"
                                 )
                             ),
                             mode = "wb")
    }, error = function(e) {
        warning(
            "The FTP site of ENSEMBLGENOMES 'ftp://ftp.ensemblgenomes.org/current/fasta' does not seem to work properly. Are you connected to the internet? Is the site 'ftp://ftp.ensemblgenomes.org/current/fasta' or 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
        )
        return(FALSE)
    })
        
    
    return(file.path(
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
    ))
}
