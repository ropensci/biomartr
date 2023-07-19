#' @title Helper function to retrieve the assembly_summary.txt file from NCBI
#' @description Retrieval function of the assembly_summary.txt file from NCBI.
#' @param db database name. E.g. \code{refseq} or \code{genbank}.
#' @param kingdom kingdom for which assembly_summary.txt file shall be 
#' retrieved. See also \code{\link{getKingdoms}}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' test <- getSummaryFile("refseq","plant")
#' test
#' }
#' @seealso \code{\link{getKingdomAssemblySummary}}, 
#' \code{\link{getMetaGenomeSummary}}
#' @export
 
getSummaryFile <- function(db,kingdom){
  
  withr::local_options(timeout = max(30000000, getOption("timeout")))
  
    if (!is.element(db, c("refseq", "genbank")))
        stop("Please select one of the available data bases: 'refseq' 
             or 'genbank'")
    
    if (!is.element(kingdom, getKingdoms(db = db)))
        stop(paste0(
            "Please select a valid kingdom: ",
            paste0(getKingdoms(db = db), collapse = ", ")
        ))
    
    if (!file.exists(file.path(
        tempdir(),
        paste0("assembly_summary_", kingdom, "_", db, ".txt")
    ))) {
        tryCatch({
            suppressMessages(downloader::download(
                paste0(
                    "ftp://ftp.ncbi.nlm.nih.gov/genomes/",
                    db,
                    "/",
                    kingdom,
                    "/assembly_summary.txt"
                ),
                destfile = file.path(
                    tempdir(),
                    paste0("assembly_summary_", kingdom, "_", db, ".txt")
                ),
                mode = "wb"
            ))
        }, error = function(e)
            message(
                "Something went wrong when trying to access the FTP site 'ftp://ftp.ncbi.nlm.nih.gov/'. Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall?. Is the the FTP site '",
                paste0(
                    "ftp://ftp.ncbi.nlm.nih.gov/genomes/",
                    db,
                    "/",
                    kingdom,
                    "/assembly_summary.txt"
                ),
                "' currently available?"
            ))
    }
    
    suppressWarnings(summary.file <-
                         tibble::as_tibble(
                             readr::read_delim(
                                 file.path(
                                     tempdir(),
                                     paste0("assembly_summary_", kingdom, "_", 
                                            db, ".txt")
                                 ),
                                 comment = "#",
                                 delim = "\t",
                                 quote = "\"",
                                 escape_double = FALSE,
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
                                     "excluded_from_refseq",
                                     "relation_to_type_material",
                                     "asm_not_live_date",
                                     "assembly_type",
                                     "group",
                                     "genome_size",
                                     "genome_size_ungapped",
                                     "gc_percent",
                                     "replicon_count",
                                     "scaffold_count",
                                     "contig_count",
                                     "annotation_provider",
                                     "annotation_name",
                                     "annotation_date",
                                     "total_gene_count",
                                     "protein_coding_gene_count",
                                     "non_coding_gene_count",
                                     "pubmed_id"
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
    )
    
    return(summary.file)
}
