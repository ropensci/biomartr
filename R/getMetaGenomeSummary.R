#' @title Retrieve the assembly_summary.txt file from NCBI genbank metagenomes
#' @description Retrieval function of the assembly_summary.txt file
#' from NCBI genbank metagenomes.
#' This files stores all available metagenome projects on NCBI Genbank.
#' @param local_file where to store this backend file, default:
#' file.path(cachedir(), "assembly_summary_metagenomes_genbank.txt")
#' @return a tibble table
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' meta.summary <- getMetaGenomeSummary()
#' meta.summary
#' }
#' @seealso \code{\link{getKingdomAssemblySummary}},
#' \code{\link{getSummaryFile}}
#' @export
getMetaGenomeSummary <- function(local_file = file.path(cachedir(), "assembly_summary_metagenomes_genbank.txt")) {
    url <- refseq_genbank_ftp_server_url_metagenome_specific("genbank")
    custom_download_check_local(url, local_file, NULL, db = "genbank")

    summary_file <- tibble::as_tibble(data.table::fread(local_file, header = TRUE))
    colnames(summary_file)[1] <- sub("#", "", colnames(summary_file)[1]) # Fix hash
    return(summary_file)
}
