#' @title Check Genome Availability
#' @description This function checks the availability of a given genome on the 
#' NBCI servers specified as scientific name.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' \item \code{db = "ensemblgenomes"}
#' \item \code{db = "uniprot"}
#' }
#' @param organism there are three options to characterize an organism: 
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param details a logical value specifying whether or not details on genome 
#' size, kingdom, etc. shall be printed to the console intead of a 
#' boolean value.
#' @details
#' Internally this function calls the \code{\link{listGenomes}} function to 
#' detect all available genomes and checks whether or not the specified organism
#' is available for download.
#' @return a logical value specifing whether or not the genome of the input 
#' organism is available. In case \code{details} = \code{TRUE} only a character 
#' string specifying the genome details is being returned.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#' # checking whether the Homo sapiens genome is stored on NCBI
#' is.genome.available(organism = "Homo sapiens", db = "refseq")
#'
#' # and printing details
#' is.genome.available(organism = "Homo sapiens", db = "refseq", details = TRUE)
#'
#' # checking whether the Homo sapiens genome is stored on ENSEMBL
#' is.genome.available(organism = "Homo sapiens", db = "ensembl")
#' 
#' # and printing details
#' is.genome.available(organism = "Homo sapiens",
#'                     details = TRUE, 
#'                     db = "ensembl")
#' }
#' @export

is.genome.available <-
    function(
             db = "refseq",
             organism,
             details = FALSE
             ) {
        if (!is.element(db, c("refseq", "genbank", 
                              "ensembl", "ensemblgenomes","uniprot")))
            stop(
                "Please select one of the available data bases: 
                'refseq', 'genbank', 'ensembl', 'ensemblgenomes', or 'uniprot'",
                call. = FALSE
            )
        
        if (is.element(db, c("refseq", "genbank"))) {
            aliases <- groups <- NULL
            # if AssemblyFilesAllKingdoms.txt file was already 
            # generated/downloaded then use the local version
            # stored in temp()
            if (file.exists(file.path(
                tempdir(),
                paste0("AssemblyFilesAllKingdoms_", db, ".txt")
            ))) {
                suppressWarnings(
                    AssemblyFilesAllKingdoms <-
                        readr::read_tsv(
                            file.path(
                                tempdir(),
                                paste0("AssemblyFilesAllKingdoms_", db, ".txt")
                            ),
                            col_names = c(
                                "assembly_accession",
                                "bioproject",
                                "biosample",
                                "wgs_master",
                                "refseq_category",
                                "taxid",
                                "species_taxid",
                                "organism_name",
                                "infraspecific_name",
                                "isolate",
                                "version_status",
                                "assembly_level",
                                "release_type",
                                "genome_rep",
                                "seq_rel_date",
                                "asm_name",
                                "submitter",
                                "gbrs_paired_asm",
                                "paired_asm_comp",
                                "ftp_path",
                                "excluded_from_refseq"
                            ),
                            comment = "#",
                            col_types = readr::cols(
                                assembly_accession = readr::col_character(),
                                bioproject = readr::col_character(),
                                biosample = readr::col_character(),
                                wgs_master = readr::col_character(),
                                refseq_category = readr::col_character(),
                                taxid = readr::col_integer(),
                                species_taxid = readr::col_integer(),
                                organism_name = readr::col_character(),
                                infraspecific_name = readr::col_character(),
                                isolate = readr::col_character(),
                                version_status = readr::col_character(),
                                assembly_level = readr::col_character(),
                                release_type = readr::col_character(),
                                genome_rep = readr::col_character(),
                                seq_rel_date = readr::col_date(),
                                asm_name = readr::col_character(),
                                submitter = readr::col_character(),
                                gbrs_paired_asm = readr::col_character(),
                                paired_asm_comp = readr::col_character(),
                                ftp_path = readr::col_character(),
                                excluded_from_refseq = readr::col_character()
                            )
                        )
                )
            } else {
                # otherwise download all assembly_summary.txt files for all 
                # kingdoms and store the AssemblyFilesAllKingdoms.txt 
                # file locally
                # retrieve the assembly_summary.txt files for all kingdoms
                kgdoms <- getKingdoms(db = db)
                storeAssemblyFiles <- vector("list", length(kgdoms))
                
                for (i in seq_along(kgdoms)) {
                    storeAssemblyFiles[i] <-
                        list(getSummaryFile(db = db, kingdom = kgdoms[i]))
                }
                
                AssemblyFilesAllKingdoms <-
                    dplyr::bind_rows(storeAssemblyFiles)
                
                readr::write_tsv(AssemblyFilesAllKingdoms,
                                 file.path(
                                     tempdir(),
                                     paste0("AssemblyFilesAllKingdoms_", 
                                            db, ".txt")
                                 ))
            }
            
            organism_name <- assembly_accession <- taxid <- NULL
            
            orgs <-
                stringr::str_replace_all(AssemblyFilesAllKingdoms$organism_name,
                                         "\\(", "")
            orgs <- stringr::str_replace_all(orgs, "\\)", "")
            
            AssemblyFilesAllKingdoms <-
                dplyr::mutate(AssemblyFilesAllKingdoms, organism_name = orgs)
            
            organism <-
                stringr::str_replace_all(organism, "\\(", "")
            organism <-
                stringr::str_replace_all(organism, "\\)", "")
            
            # test if organism specification could be a taxid
            if (!is.taxid(organism)) {
                FoundOrganism <-
                    dplyr::filter(AssemblyFilesAllKingdoms,
                                  stringr::str_detect(organism_name, organism) |
                                      stringr::str_detect(assembly_accession, organism))
                
            } else {
                FoundOrganism <-
                    dplyr::filter(AssemblyFilesAllKingdoms,
                                  taxid == as.integer(organism))
            }
            
            if (nrow(FoundOrganism) == 0) {
                message(
                    "Unfortunatey, no entry for '",
                    organism,
                    "' was found in the '",
                    db,
                    "' database. ",
                    "Please consider specifying ",
                    paste0("'db = ", dplyr::setdiff(
                        c("refseq", "genbank", "ensembl", "ensemblgenomes", "uniprot"), db
                    ), collapse = "' or "),
                    "' to check whether '",organism,"' is available in these databases."
                )
                return(FALSE)
            }
            
            if (nrow(FoundOrganism) > 0) {
                if (!details) {
                    
                    if (all(FoundOrganism$refseq_category == "na")) {
                        message("Only a non-reference genome assembly is available for '", organism, "'.",
                                " Please make sure to specify the argument 'reference = FALSE' when running any get*() function.")
                    } else {
                        message("A reference or representative genome assembly is available for '", organism, "'.")
                    }
                    
                    if (nrow(FoundOrganism) > 1) {
                        message("More than one entry was found for '", organism, "'.",
                                " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
                                organism, ", db = ",db, ", details = TRUE)'.",
                                " This will allow you to select the 'assembly_accession' identifier that can then be ",
                                "specified in all get*() functions.")
                    }
                    return(TRUE)
                }
                    
                
                if (details) {
                    if (all(FoundOrganism$refseq_category == "na")) {
                        message("Only a non-reference genome assembly is available for '", organism, "'.",
                                " Please make sure to specify the argument 'reference = FALSE' when running any get*() function.")
                    }
                    return(FoundOrganism)
                }
            }
        }
        
        if (db == "ensembl") {
            new.organism <- stringr::str_replace_all(organism, " ", "_")
            
            if (file.exists(file.path(tempdir(), "ensembl_summary.txt"))) {
                suppressWarnings(
                    ensembl.available.organisms <-
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
                        )
                )
            }
            
            if (!file.exists(file.path(tempdir(), "ensembl_summary.txt"))) {
                # check if organism is available on ENSEMBL
                tryCatch({
                    ensembl.available.organisms <-
                        jsonlite::fromJSON(
            "http://rest.ensembl.org/info/species?content-type=application/json"
                        )
                }, error = function(e)
                    stop(
                        "The API 'http://rest.ensembl.org' does not seem to work ",
                        "properly. Are you connected to the internet? Is the ",
                       "homepage 'http://rest.ensembl.org' currently available?",
                        call. = FALSE
                    ))
                
                # transform list object returned by 'fromJSON' to tibble
                ensembl.available.organisms <-
                    tibble::as_tibble(
                        dplyr::select(
                            ensembl.available.organisms$species,
                            -aliases,
                            -groups
                        )
                    )
                
                readr::write_tsv(
                    ensembl.available.organisms,
                    file.path(tempdir(), "ensembl_summary.txt")
                )
            }
            
            name <- accession <- assembly <- taxon_id <- NULL
            
            if (!is.taxid(organism)) {
                selected.organism <-
                    dplyr::filter(
                        ensembl.available.organisms,
                        stringr::str_detect(name, 
                                            stringr::str_to_lower(new.organism)) |
                            stringr::str_detect(accession, organism), !is.na(assembly)
                    )
            } else {
                selected.organism <-
                    dplyr::filter(
                        ensembl.available.organisms, taxon_id == organism, !is.na(assembly))
                
            }
            
            
            if (!details) {
                
                if (nrow(selected.organism) == 0) {
                    message(
                        "Unfortunatey, no entry for '",
                        organism,
                        "' was found in the '",
                        db,
                        "' database. ",
                        "Please consider specifying ",
                        paste0("'db = ", dplyr::setdiff(
                            c("refseq", "genbank", "ensembl", "ensemblgenomes", "uniprot"), db
                        ), collapse = "' or "),
                        "' to check whether '",organism,"' is available in these databases."
                    )
                    return(FALSE)
                }
                    
                
                if (nrow(selected.organism) > 0) {
                    message("A reference or representative genome assembly is available for '", organism, "'.")
                    if (nrow(selected.organism) > 1) {
                        message("More than one entry was found for '", organism, "'.",
                                " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
                                organism, ", db = ",db, ", details = TRUE)'.",
                                " This will allow you to select the 'assembly_accession' identifier that can then be ",
                                "specified in all get*() functions.")
                    }
                    return(TRUE)
                }
                    
            }
            
            if (details)
                return(selected.organism)
        }
        
        if (db == "ensemblgenomes") {
            new.organism <- stringr::str_replace_all(organism, " ", "_")
            
            if (file.exists(file.path(tempdir(), 
                                      "ensemblgenomes_summary.txt"))) {
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
            
            if (!file.exists(file.path(tempdir(), 
                                       "ensemblgenomes_summary.txt"))) {
                # check if organism is available on ENSEMBL
                tryCatch({
                    ensembl.available.organisms <-
                        jsonlite::fromJSON(
     "http://rest.ensemblgenomes.org/info/species?content-type=application/json"
                        )
                }, error = function(e)
                    stop(
                        "The API 'http://rest.ensemblgenomes.org' does not seem ",
                        "to work properly. Are you connected to the internet? ",
                        "Is the homepage 'http://rest.ensemblgenomes.org' ",
                        "currently available?", call. = FALSE
                    ))
                
                # transform list object returned by 'fromJSON' to tibble
                ensembl.available.organisms <-
                    tibble::as_tibble(
                        dplyr::select(
                            ensembl.available.organisms$species,
                            -aliases,
                            -groups
                        )
                    )
                
                readr::write_tsv(
                    ensembl.available.organisms,
                    file.path(tempdir(), "ensemblgenomes_summary.txt")
                )
            }
            
     
            name <- accession <- taxon_id <- NULL
            
            if (!is.taxid(organism)) {
                selected.organism <-
                    dplyr::filter(
                        ensembl.available.organisms,
                        stringr::str_detect(name, 
                                            stringr::str_to_lower(new.organism)) |
                            stringr::str_detect(accession, organism), !is.na(assembly)
                    )
            } else {
                selected.organism <-
                    dplyr::filter(
                        ensembl.available.organisms, taxon_id == organism, !is.na(assembly))
                
            }
            
            
            if (!details) {
                
                if (nrow(selected.organism) == 0) {
                    message(
                        "Unfortunatey, no entry for '",
                        organism,
                        "' was found in the '",
                        db,
                        "' database. ",
                        "Please consider specifying ",
                        paste0("'db = ", dplyr::setdiff(
                            c("refseq", "genbank", "ensembl", "ensemblgenomes", "uniprot"), db
                        ), collapse = "' or "),
                        "' to check whether '",organism,"' is available in these databases."
                    )
                    return(FALSE)
                }
                
                
                if (nrow(selected.organism) > 0) {
                    message("A reference or representative genome assembly is available for '", organism, "'.")
                    if (nrow(selected.organism) > 1) {
                        message("More than one entry was found for '", organism, "'.",
                                " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
                                organism, ", db = ",db, ", details = TRUE)'.",
                                " This will allow you to select the 'assembly_accession' identifier that can then be ",
                                "specified in all get*() functions.")
                    }
                    return(TRUE)
                }
                
            }
            
            if (details)
                return(selected.organism)
        }
        
        if (db == "uniprot") {
            
            organism_new <- stringr::str_replace_all(organism, " ", "%20")
            
            tryCatch({
                uniprot_species_info <-
                    tibble::as_tibble(jsonlite::fromJSON(
                        paste0(
                            "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&name=",
                            organism_new
                        )
                    ))
            }, error = function(e)
                stop(
                    "The API 'https://www.ebi.ac.uk/proteins/api/proteomes'",
                    " does not seem to work properly. Are you connected to the ", " internet? Is the homepage 'https://www.ebi.ac.uk/' currently available?",
                    call. = FALSE
                ))
            
            if (!details) {
                if (nrow(uniprot_species_info) == 0) {
                    message(
                        "Unfortunatey, no entry for '",
                        organism,
                        "' was found in the '",
                        db,
                        "' database. ",
                        "Please consider specifying ",
                        paste0("'db = ", dplyr::setdiff(
                            c(
                                "refseq",
                                "genbank",
                                "ensembl",
                                "ensemblgenomes",
                                "uniprot"
                            ),
                            db
                        ), collapse = "' or "),
                        "' to check whether '",
                        organism,
                        "' is available in these databases."
                    )
                    return(FALSE)
                }
                
                if (nrow(uniprot_species_info) > 0) {
                    message(
                        "A reference or representative genome assembly is available for '",
                        organism,
                        "'."
                    )
                    if (nrow(uniprot_species_info) > 1) {
                        message(
                            "More than one entry was found for '",
                            organism,
                            "'.",
                            " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
                            organism,
                            ", db = ",
                            db,
                            ", details = TRUE)'.",
                            " This will allow you to select the 'assembly_accession' identifier that can then be ",
                            "specified in all get*() functions."
                        )
                    }
                    return(TRUE)
                }
                
            }
        }
        
        
        if (details)
            return(uniprot_species_info)
        
    }
