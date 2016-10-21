getENSEMBL.Annotation <- function(organism, type = "dna", id.type = "toplevel", path) {
    
    if (!is.element(type, c("dna", "cds", "pep")))
        stop("Please a 'type' argument supported by this function: 'dna', 'cds', 'pep'.")
    
    # test if REST API is responding
    is.ensembl.alive()
    
    new.organism <- stringr::str_replace(organism, " ", "_")
    
    # check if organism is available on ENSEMBL
    tryCatch({
        ensembl.available.organisms <-
            jsonlite::fromJSON("http://rest.ensembl.org/info/species?content-type=application/json")
    }, error = function(e)
        stop(
            "The API 'http://rest.ensembl.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensembl.org' currently available?",call. = FALSE
        ))
    
    if (!is.element(stringr::str_to_lower(new.organism),
                    ensembl.available.organisms$species$name))
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
    
    # construct retrieval query
    ensembl.qry <-
        paste0(
            "ftp://ftp.ensembl.org/pub/current_gff3/",
            stringr::str_to_lower(new.organism),
            "/",
            paste0(
                new.organism,
                ".",
                json.qry.info$default_coord_system_version,
                ".",
                ensembl.available.organisms$species$release[1],
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
                                     ensembl.available.organisms$species$release[1],
                                     "_ensembl",
                                     ".gff3.gz"
                                 )
                             ),
                             mode = "wb")
    }, error = function(e)
        stop(
            "The FTP site of ENSEMBL 'ftp://ftp.ensembl.org/current_gff3/' does not seem to work properly. Are you connected to the internet? Is the site 'ftp://ftp.ensembl.org/current_gff3/' or 'http://rest.ensembl.org' currently available?", call. = FALSE
        ))
    
    return(file.path(
        path,
        paste0(
            new.organism,
            ".",
            json.qry.info$default_coord_system_version,
            ".",
            ensembl.available.organisms$species$release[1],
            "_ensembl",
            ".gff3.gz"
        )
    ))
}
