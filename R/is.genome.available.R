#' @title Check Genome Availability
#' @description This function checks the availability of a given genome on the NBCI servers specified
#' as scientific name.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param details a logical value specifying whether or not details on genome size, kingdom, etc. shall be printed to the
#' console intead of a boolean value.
#' @param db a character string specifying the database for which genome availability shall be checked,
#' e.g. \code{db = "refseq"},\code{db = "genbank"}, \code{db = "ensembl"}, \code{db = "ensemblgenomes"}, or \code{db =  "all"}.
#' @details
#'
#' Internally this function calls the \code{\link{listGenomes}} function to detect all available genomes
#' and checks whether or not the specified organism is available for download.
#'
#' @return a logical value specifing whether or not the genome of the input organism
#' is available. In case \code{details} = \code{TRUE} only a character string specifying the
#' genome details is being returned.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#'
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
#' is.genome.available(organism = "Homo sapiens", details = TRUE, db = "ensembl")
#' }
#' @references
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt}
#' @export

is.genome.available <- function(organism, details = FALSE, db = "refseq"){
    
    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop("Please select one of the available data bases: 'refseq', 'genbank', or 'ensembl'",
             call. = FALSE)
    
    if (is.element(db, c("refseq", "genbank"))) {
        # if AssemblyFilesAllKingdoms.txt file was already generated/downloaded then use the local version
        # stored in temp()
        if (file.exists(file.path(tempdir(), paste0("AssemblyFilesAllKingdoms_", db, ".txt")))) {
            suppressWarnings(
                AssemblyFilesAllKingdoms <-
                    readr::read_tsv(
                        file.path(tempdir(), paste0("AssemblyFilesAllKingdoms_", db, ".txt")),
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
            # otherwise download all assembly_summary.txt files for all kingdoms and store the AssemblyFilesAllKingdoms.txt file locally
            # retrieve the assembly_summary.txt files for all kingdoms
            kgdoms <- getKingdoms(db = db)
            storeAssemblyFiles <- vector("list", length(kgdoms))
            
            for (i in seq_along(kgdoms)) {
                storeAssemblyFiles[i] <-
                    list(getSummaryFile(db = db, kingdom = kgdoms[i]))
            }
            
            AssemblyFilesAllKingdoms <-
                dplyr::bind_rows(storeAssemblyFiles)
            
            readr::write_tsv(
                AssemblyFilesAllKingdoms,
                file.path(tempdir(), paste0("AssemblyFilesAllKingdoms_", db, ".txt"))
            )
        }
        
        organism_name <- NULL
        
        FoundOrganism <-
            dplyr::filter(AssemblyFilesAllKingdoms,
                          stringr::str_detect(organism_name, organism))
        
        if (nrow(FoundOrganism) == 0)
            stop("Unfortunately no entry for organism '",
                 organism,
                 "' could be found.",
                 call. = FALSE)
        
        available_genome <- listGenomes("all", TRUE, db = "all")
        
        is_available <- any(stringr::str_detect(available_genome[, "organism_name"], organism))
        
        
        if (is_available) {
            organism_index <-
                which(stringr::str_detect(available_genome[, "organism_name"], organism))
            
            if (details) {
                return(available_genome[organism_index,])
                
            } else {
                return(TRUE)
            }
            
        } else {
            return(FALSE)
        }
    }
    
    if (db == "ensembl") {
        
        new.organism <- stringr::str_replace(organism," ","_")
    
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
                "' is not available at ENSEMBL. Please check whether or not the organism name is typed correctly or try to use db = 'ensemblgenomes'.", call. = FALSE
            )
        
        name <- NULL
        selected.organism <- dplyr::filter(ensembl.available.organisms,
                                           name == stringr::str_to_lower(new.organism))
        
        if (!details) {
            if (nrow(selected.organism) > 0)
                return(TRUE)
            
            if (nrow(selected.organism) == 0)
                return(FALSE)
        }
        
        if (details)
            return(selected.organism)
    }
    
    if (db == "ensemblgenomes") {
        
        new.organism <- stringr::str_replace(organism," ","_")
        
        if (file.exists(file.path(tempdir(), "ensemblgenomes_summary.txt"))) {
            suppressWarnings(ensembl.available.organisms <-
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
                ))
        }
        
        if (!file.exists(file.path(tempdir(), "ensemblgenomes_summary.txt"))) {
        # check if organism is available on ENSEMBL
        tryCatch({
            ensembl.available.organisms <-
                jsonlite::fromJSON("http://rest.ensemblgenomes.org/info/species?content-type=application/json")
        }, error = function(e)
            stop(
                "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?"
            ))
        
        # transform list object returned by 'fromJSON' to tibble
        ensembl.available.organisms <-
            tibble::as_tibble(dplyr::select(ensembl.available.organisms$species, -aliases, -groups))
        
        readr::write_tsv(ensembl.available.organisms,
                         file.path(tempdir(), "ensemblgenomes_summary.txt"))
        }
        
        if (!is.element(stringr::str_to_lower(new.organism),ensembl.available.organisms$name))
            stop("Unfortunately organism '",organism,"' is not available at ENSEMBLGENOMES. Please check whether or not the organism name is typed correctly.", call. = FALSE)
        
        name <- NULL
        selected.organism <- dplyr::filter(ensembl.available.organisms, name == stringr::str_to_lower(new.organism))
        
        if (!details) {
            if (nrow(selected.organism) > 0)
                return(TRUE)
            
            if (nrow(selected.organism) == 0)
                return(FALSE)
        }
        
        if (details)
            return(selected.organism)
    }
}
