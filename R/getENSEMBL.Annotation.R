getENSEMBL.Annotation <- function(organism, type = "dna", id.type = "toplevel", path) {
    
    if (!is.element(type, c("dna", "cds", "pep")))
        stop("Please a 'type' argument supported by this function: 'dna', 'cds', 'pep'.")
    
    # test if REST API is responding
    is.ensembl.alive()
    
    new.organism <- stringr::str_replace(organism, " ", "_")
    
    if (file.exists(file.path(tempdir(), "ensembl_summary.txt"))) {
        suppressWarnings(ensembl.available.organisms <-
                             readr::read_tsv(
                                 file.path(tempdir(), "ensembl_summary.txt"),
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
                jsonlite::fromJSON("http://rest.ensembl.org/info/species?content-type=application/json")
        }, error = function(e)
            stop(
                "The API 'http://rest.ensembl.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensembl.org' currently available?", call. = FALSE
            ))
        
        # transform list object returned by 'fromJSON' to tibble
        ensembl.available.organisms <-
            tibble::as_tibble(dplyr::select(ensembl.available.organisms$species, -aliases, -groups))
        
        readr::write_tsv(ensembl.available.organisms,
                         file.path(tempdir(), "ensembl_summary.txt"))
    }
    
    if (!is.element(stringr::str_to_lower(new.organism),
                    ensembl.available.organisms$name))
        stop(
            "Unfortunately organism '",
            organism,
            "' is not available at ENSEMBL. Please check whether or not the organism name is typed correctly.", call. = FALSE
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
        stop(
            "The API 'http://rest.ensembl.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensembl.org' currently available?", call. = FALSE
        ))
    
    # retrieve detailed information for organism of interest
    get.org.info <- is.genome.available(organism = organism, details = TRUE, db = "ensembl")
    
    # retrieve the Ensembl Genomes version of the databases backing this service
    eg_version <- jsonlite::fromJSON("http://rest.ensembl.org/info/eg_version?content-type=application/json")
    
    # construct retrieval query
    ensembl.qry <-
        paste0(
            "ftp://ftp.ensembl.org/pub/current/",
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
                                     "_ensembl",
                                     ".gff3.gz"
                                 )
                             ),
                             mode = "wb")
    }, error = function(e)
        stop(
            "The FTP site of ENSEMBL 'ftp://ftp.ensembl.org/current/gff3' does not seem to work properly. Are you connected to the internet? Is the site 'ftp://ftp.ensembl.org/current/gff3' or 'http://rest.ensembl.org' currently available?", call. = FALSE
        ))
    
    return(file.path(
        path,
        paste0(
            new.organism,
            ".",
            json.qry.info$default_coord_system_version,
            ".",
            eg_version,
            "_ensembl",
            ".gff3.gz"
        )
    ))
}
