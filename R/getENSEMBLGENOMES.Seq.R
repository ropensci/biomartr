getENSEMBLGENOMES.Seq <- function(organism, type = "dna", id.type = "toplevel", path) {
    
    if (!is.element(type, c("dna", "cds", "pep")))
        stop("Please a 'type' argument supported by this function: 'dna', 'cds', 'pep'.")
    
    new.organism <- stringr::str_replace(organism, " ", "_")
    
    # test if REST API is responding
    is.ensemblgenomes.alive()
    
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
                jsonlite::fromJSON(
                    "http://rest.ensemblgenomes.org/info/species?content-type=application/json"
                )
        }, error = function(e)
            stop(
                "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
            ))
        # transform list object returned by 'fromJSON' to tibble
        ensembl.available.organisms <-
            tibble::as_tibble(dplyr::select(ensembl.available.organisms$species, -aliases, -groups))
        
        readr::write_tsv(
            ensembl.available.organisms,
            file.path(tempdir(), "ensemblgenomes_summary.txt")
        )
    }
    
    if (!is.element(stringr::str_to_lower(new.organism),
                    ensembl.available.organisms$name))
        stop(
            "Unfortunately organism '",
            organism,
            "' is not available at ENSEMBLGENOMES. Please check whether or not the organism name is typed correctly.", call. = FALSE
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
            "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?"
        ))
    
    # construct retrieval query
    ensembl.qry <-
        paste0(
            "ftp://ftp.ensemblgenomes.org/pub/current_fasta/",
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
                ".",
                id.type,
                ".fa.gz"
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
                                     type,
                                     ".",
                                     id.type,
                                     ".fa.gz"
                                 )
                             ),
                             mode = "wb")
    }, error = function(e)
        stop(
            "The FTP site of ENSEMBL 'ftp://ftp.ensemblgenomes.org/pub/' does not seem to work properly. Are you connected to the internet? Is the site 'ftp://ftp.ensemblgenomes.org/pub/' or 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
        ))
    
    return(file.path(
        path,
        paste0(
            new.organism,
            ".",
            json.qry.info$default_coord_system_version,
            ".",
            type,
            ".",
            id.type,
            ".fa.gz"
        )
    ))
}
