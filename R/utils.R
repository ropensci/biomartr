
#' @title Check Genome Availability
#' @description This function checks the availability of a given genome on the NBCI servers specified
#' as scientific name.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis thaliana'.
#' @param details a logical value specifying whether or not details on genome size, kingdom, etc. shall be printed to the
#' console intead of a boolean value.
#' @param db a character string specifying the database for which genome availability shall be checked,
#' e.g. \code{db} =  \code{"refseq"} or \code{db} =  \code{"all"}.
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
#' # checking whether the Arabidopsis thaliana genome is stored on NCBI
#' is.genome.available(organism = "Arabidopsis thaliana")
#'
#' # and printing details
#' is.genome.available(organism = "Arabidopsis thaliana", details = TRUE)
#'
#' }
#' @references
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt}
#' @export

is.genome.available <- function(organism, details = FALSE, db = "refseq"){

    if (!is.element(db, c("refseq", "genbank")))
        stop("Please select one of the available data bases: 'refseq' or 'genbank'",
             call. = FALSE)
    
    # if AssemblyFilesAllKingdoms.txt file was already generated/downloaded then use the local version
    # stored in temp()
    if (file.exists(file.path(tempdir(), "AssemblyFilesAllKingdoms.txt"))) {
        suppressWarnings(
            AssemblyFilesAllKingdoms <-
                readr::read_tsv(
                    file.path(tempdir(), "AssemblyFilesAllKingdoms.txt"),
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
        kgdoms <- getKingdoms()
        storeAssemblyFiles <- vector("list", length(kgdoms))
        
        for (i in seq_along(kgdoms)) {
            storeAssemblyFiles[i] <-
                list(getSummaryFile(db = db, kingdom = kgdoms[i]))
        }
        
        AssemblyFilesAllKingdoms <-
            dplyr::bind_rows(storeAssemblyFiles)
        
        readr::write_tsv(
            AssemblyFilesAllKingdoms,
            file.path(tempdir(), "AssemblyFilesAllKingdoms.txt")
        )
    }
    
    organism_name <- NULL
    FoundOrganism <- dplyr::filter(AssemblyFilesAllKingdoms, organism_name == organism)
    
    if (nrow(FoundOrganism) == 0)
        stop("Unfortunately no entry for organism '",
             organism,
             "' could not be found.",
             call. = FALSE)
    
    available_genome <- listGenomes("all", TRUE, db = db)
    
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

docFile <- function(file.name, 
                    organism, 
                    url, 
                    database, 
                    path, 
                    refseq_category, 
                    assembly_accession,
                    bioproject,
                    biosample,
                    taxid,
                    infraspecific_name,
                    version_status,
                    release_type,
                    genome_rep,
                    seq_rel_date,
                    submitter){

        cwd <- getwd()

        setwd(path)

        sink(paste0("doc_",organism,"_db_",database,".txt"))

        cat(paste0("File Name: ", file.name))
        cat("\n")
        cat(paste0("Organism Name: ", organism))
        cat("\n")
        cat(paste0("Database: NCBI ", database))
        cat("\n")
        cat(paste0("URL: ", url))
        cat("\n")
        cat(paste0("Download_Date: ", date()))
        cat("\n")
        cat(paste0("refseq_category: ", refseq_category))
        cat("\n")
        cat(paste0("assembly_accession: ", assembly_accession))
        cat("\n")
        cat(paste0("bioproject: ", bioproject))
        cat("\n")
        cat(paste0("biosample: ", biosample))
        cat("\n")
        cat(paste0("taxid: ", taxid))
        cat("\n")
        cat(paste0("infraspecific_name: ", infraspecific_name))
        cat("\n")
        cat(paste0("version_status: ", version_status))
        cat("\n")
        cat(paste0("release_type: ", release_type))
        cat("\n")
        cat(paste0("genome_rep: ", genome_rep))
        cat("\n")
        cat(paste0("seq_rel_date: ", seq_rel_date))
        cat("\n")
        cat(paste0("submitter: ", submitter))
        
        sink()

        setwd(cwd)

}



setTMPFile <- function(file.name){

        tempfile(pattern = file.name, tmpdir = tempdir(), fileext = "")
}


getTMPFile <- function(file.name){

        file.path(tempdir(),file.name)

}



test <- function(x){ print(paste0("Test ",x," passed.","\n"))}
