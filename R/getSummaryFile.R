#' @title Helper function to retrieve the assembly_summary.txt file from NCBI
#' @description Retrieval function of the assembly_summary.txt file from NCBI.
#' @param db database name. E.g. \code{refseq} or \code{genbank}.
#' @param kingdom kingdom for which assembly_summary.txt file shall be retrieved. See also \code{\link{getKingdoms}}.
#' @author Hajk-Georg Drost
#' @examples 
#' test <- getSummaryFile("refseq","plant")
#' test
#' @export
 
getSummaryFile <- function(db,kingdom){
    
    if (!is.element(db,c("refseq","genbank")))
        stop("Please select one of the available data bases: 'refseq' or 'genbank'")
    
    if (!is.element(kingdom,getKingdoms()))
        stop(paste0("Please select a valid kingdom: ",paste0(getKingdoms(),collapse = ", ")))
    
    downloader::download(
        paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/",db,"/",kingdom,"/assembly_summary.txt"),
        destfile = file.path(tempdir(), paste0("assembly_summary_", kingdom, "_", db, ".txt"))
    )
    summary.file <-
        tibble::as_tibble(readr::read_tsv(
            file.path(tempdir(), paste0("assembly_summary_", kingdom, "_", db, ".txt")),
            comment = "#",
            col_names = c("assembly_accession", "bioproject", "biosample",
                          "wgs_master", "refseq_category", "taxid",
                          "species_taxid", "organism_name", "infraspecific_name",
                          "isolate", "version_status", "assembly_level",
                          "release_type", "genome_rep", "seq_rel_date",
                          "asm_name", "submitter", "gbrs_paired_asm",
                          "paired_asm_comp", "ftp_path", "excluded_from_refseq"),
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
                ftp_path = readr::col_character()
            )
          )
        )
    
    return(summary.file)
}
