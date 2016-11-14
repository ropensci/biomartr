#' @title Helper function to retrieve the assembly_summary.txt files  from NCBI for all kingdoms
#' @description Retrieval function of the assembly_summary.txt file from NCBI for all kingdoms.
#' @param db database name. E.g. \code{refseq} or \code{genbank}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' test <- getKingdomAssemblySummary(db = "refseq")
#' test
#' }
#' @seealso \code{\link{getSummaryFile}}, \code{\link{getMetaGenomeSummary}} 
#' @export
getKingdomAssemblySummary <- function(db) {
    
    if (!is.element(db, c("refseq", "genbank")))
        stop ("Please select one of the available data bases: 'refseq' or 'genbank'")
    
    if (is.element(db, c("refseq", "genbank"))) {
        # if AssemblyFilesAllKingdoms.txt file was already generated/downloaded then use the local version
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
                        comment = "#",
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
            
            readr::write_tsv(AssemblyFilesAllKingdoms,
                             file.path(
                                 tempdir(),
                                 paste0("AssemblyFilesAllKingdoms_", db, ".txt")
                             ))
        }
    }
    
    orgs <- stringr::str_replace_all(AssemblyFilesAllKingdoms$organism_name,"\\(","")
    orgs <- stringr::str_replace_all(orgs,"\\)","")
    
    AssemblyFilesAllKingdoms <- dplyr::mutate(AssemblyFilesAllKingdoms, organism_name = orgs)
    return(AssemblyFilesAllKingdoms)
}
