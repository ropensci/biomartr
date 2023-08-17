#' @title Retrieve and summarise the assembly_summary.txt files from 
#' NCBI for all kingdoms
#' @description Retrieval function of the assembly_summary.txt file 
#' from NCBI for all kingdoms.
#' The assembly_summary.txt files store available species on NCBI. 
#' @param db database name. E.g. \code{refseq} or \code{genbank}.
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023), 
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the 
#' bacterial summary file will be downloaded.    
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # This example will run the default version of this function 
#' # whereby information for Bacteria are not downloaded
#' test <- getKingdomAssemblySummary(db = "genbank", skip_bacteria = TRUE)
#' test
#' # Users can then retrieve information for Bacteria by skip_bacteria = FALSE
#' test2 <- getKingdomAssemblySummary(db = "genbank", skip_bacteria = FALSE)
#' test2
#' }
#' @seealso \code{\link{getSummaryFile}}, \code{\link{getMetaGenomeSummary}} 
#' @export

getKingdomAssemblySummary <- function(db, skip_bacteria = TRUE) {
    if (!is.element(db, c("refseq", "genbank")))
        stop("Please select one of the available data bases:
             'refseq' or 'genbank'")
    
    if (is.element(db, c("refseq", "genbank"))) {
        # if AssemblyFilesAllKingdoms.txt file was already generated/downloaded
        # then use the local version stored in temp()
        if (file.exists(file.path(
            tempdir(),
            paste0("AssemblyFilesAllKingdoms_", db, ".txt")
        )) & skip_bacteria) {
            suppressWarnings(
                AssemblyFilesAllKingdoms <-
                    readr::read_delim(
                        file.path(
                            tempdir(),
                            paste0("AssemblyFilesAllKingdoms_", db, ".txt")
                        ),
                        comment = "#",
                        delim = "\t",
                        quote = "\"",
                        escape_backslash = FALSE,
                        col_names = TRUE,
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
                            excluded_from_refseq = readr::col_character(),
                            relation_to_type_material = readr::col_character(),
                            asm_not_live_date = readr::col_character(),
                            assembly_type = readr::col_character(),
                            group = readr::col_character(),
                            genome_size = readr::col_integer(),
                            genome_size_ungapped = readr::col_integer(),
                            gc_percent = readr::col_double(),
                            replicon_count = readr::col_integer(),
                            scaffold_count = readr::col_integer(),
                            contig_count = readr::col_integer(),
                            annotation_provider = readr::col_character(),
                            annotation_name = readr::col_character(),
                            annotation_date = readr::col_date(format = "%m/%d/%y"),
                            total_gene_count = readr::col_integer(),
                            protein_coding_gene_count = readr::col_integer(),
                            non_coding_gene_count = readr::col_integer(),
                            pubmed_id = readr::col_character()
                        )
                    )
            )
        } else {
            # otherwise download all assembly_summary.txt files for all kingdoms
            # and store the AssemblyFilesAllKingdoms.txt file locally
            # retrieve the assembly_summary.txt files for all kingdoms
            cat("It seems that this is the first time you run this command for",db,".")
            cat("\n")
            message("Thus, 'assembly_summary.txt' files for all kingdoms will be retrieved from ",db,". ")
            message("Don't worry this has to be done only once if you don't restart your R session.")
            message("\n")
            kgdoms <- getKingdoms(db = db)
            storeAssemblyFiles <- vector("list", length(kgdoms))
            
            if (skip_bacteria){
                cat( "Due to its extended dataset size (>700 MB) the GenBank Kingdom 'bacteria' will not be downloaded by default anymore. To also include 'bacteria' please specify the argument 'skip_bacteria = FALSE'" )
                cat("\n")
                cat("\n")
            }
                
            
            for (i in seq_along(kgdoms)) {
                if (kgdoms[i] == "bacteria" && db == "genbank" && skip_bacteria){
                    cat("--------> Skipping bacteria download .....")
                    cat("\n")
                    cat("\n")
                } else {
                    cat("-> Starting download for:", kgdoms[i])
                    cat("\n")
                    storeAssemblyFiles[i] <-
                        list(getSummaryFile(db = db, kingdom = kgdoms[i]))
                }
            }
            
            AssemblyFilesAllKingdoms <-
                dplyr::bind_rows(storeAssemblyFiles)
            
            readr::write_tsv(AssemblyFilesAllKingdoms,
                             file.path(
                                 tempdir(),
                                 paste0("AssemblyFilesAllKingdoms_", db, ".txt")
                             ))
            
            message("\n")
            message("Completed!")
            message("Now continue with species download ...")
        }
    }
    
    orgs <-
        stringr::str_replace_all(AssemblyFilesAllKingdoms$organism_name, 
                                 "\\(", "")
    orgs <- stringr::str_replace_all(orgs, "\\)", "")
    
    AssemblyFilesAllKingdoms <-
        dplyr::mutate(AssemblyFilesAllKingdoms, organism_name = orgs)
    
    return(AssemblyFilesAllKingdoms)
}
