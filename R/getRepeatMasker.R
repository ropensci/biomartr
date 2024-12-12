#' @title Repeat Masker Retrieval
#' @description  Main Repeat Masker output retrieval function for an
#' organism of interest.
#' By specifying the scientific name of an organism of interest the
#' corresponding Repeat Masker file storing the genome of the organism of
#' interest can be downloaded and stored locally.
#' Repeat Masker files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' }
#' @param organism a character string specifying the scientific name of the
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @param release most recent database version is used. release = 75 would for human would give the stable GRCh37 release in ensembl.
#' Value must be > 46, since ensembl did not structure their data if the standard format before that.
#' @param gunzip a logical, indicating whether or not files should be unzipped.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding file shall be stored. Default is
#' \code{path} = \code{file.path("_ncbi_downloads","repeatmasker")}.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: \code{refseq_genbank_ftp_server_url_genome_specific("refseq")}
#'
#'  genbank: \code{refseq_genbank_ftp_server_url_genome_specific("genbank")}
#'
#' and creates a directory '_ncbi_downloads/repeatmasker' to store
#' the files of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/repeatmasker' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded Repeat Masker output file.
#' @examples \dontrun{
#'
#' # download the Repeat Masker output file of Homo sapiens from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' file_path <- getRepeatMasker( db       = "refseq",
#'              organism = "Homo sapiens",
#'              path = file.path("_ncbi_downloads","repeatmasker"))
#'
#' Hsap_repeatmasker <- read_rm(file_path)
#'
#' }
#'
#' @seealso \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{getCollection}}, \code{\link{meta.retrieval}},
#' \code{\link{read_rm}}
#' @export

getRepeatMasker <-
    function(db = "refseq",
             organism,
             reference = FALSE,
             skip_bacteria = TRUE,
             release = NULL,
             gunzip = FALSE,
             path = file.path("_ncbi_downloads", "repeatmasker"),
             mute_citation = FALSE) {

    if (!is.element(db, c("refseq", "genbank")))
            stop(
                "Please select one of the available data bases for which ",
                "Repeat Masker output files are available: 'refseq' or ",
                "'genbank'.",
                call. = FALSE
            )

        message("-> Starting Repeat Masker retrieval of '", organism, "' from ", db, " ...")
        message("\n")

        if (is.element(db, c("refseq", "genbank"))) {
          info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                          release, gunzip = gunzip, path, type = "repeat_masker")
          refseq_genbank_download_post_processing(info, organism, db, path,
                                                  gunzip = gunzip,
                                                  remove_annotation_outliers = FALSE,
                                                  format = "repeat_masker",
                                                  mute_citation = mute_citation)
        }
}





