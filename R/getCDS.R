#' @title Coding Sequence Retrieval
#' @description Main retrieval function for coding sequences (CDS)
#' of an organism of interest.
#' By specifying the scientific name of an organism of interest the
#' corresponding fasta-file storing the CDS information for the organism
#' of interest can be downloaded and stored locally. CDS files can be retrieved
#' from several databases.
#' @inheritParams getGenome
#' @param path a character string specifying the location (a folder)
#' in which the corresponding CDS file shall be stored.
#' Default is \code{path} = \code{file.path("_ncbi_downloads","CDS")}.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @return File path to downloaded CDS file.
#' @examples
#' \dontrun{
#' # download the genome of Arabidopsis thaliana from refseq
#' # and store the corresponding genome CDS file in '_ncbi_downloads/CDS'
#' file_path <- getCDS( db       = "refseq",
#'              organism = "Arabidopsis thaliana",
#'              path     = file.path("_ncbi_downloads","CDS"))
#'
#' Ath_CDS <- read_cds(file_path, format = "fasta")
#'
#' }
#' @seealso \code{\link{getGenome}}, \code{\link{getProteome}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{getRepeatMasker}},
#' \code{\link{getAssemblyStats}}, \code{\link{getCollection}}, \code{\link{meta.retrieval}},
#' \code{\link{read_cds}}
#' @export

getCDS <-
    function(db = "refseq",
             organism,
             reference = FALSE,
             skip_bacteria = TRUE,
             release = NULL,
             gunzip = FALSE,
             path = file.path("_ncbi_downloads", "CDS"),
             mute_citation = FALSE) {

    if (!is.element(db, c("refseq", "genbank",
                          "ensembl")))
        stop(
            "Please select one of the available data bases:
            'refseq', 'genbank', or 'ensembl'.",
            call. = FALSE
        )

    message("-> Starting CDS retrieval of '", organism, "' from ", db, " ...")
    message("\n")

    # create result folder
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    if (is.element(db, c("refseq", "genbank"))) {
      info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                      release, gunzip, path, type = "cds")
      refseq_genbank_download_post_processing(info, organism, db, path,
                                              gunzip,
                                              remove_annotation_outliers,
                                              format = "cds",
                                              mute_citation = mute_citation)
    } else if (db %in% c("ensembl", "ensemblgenomes")) {
        # download CDS sequence from ENSEMBL
        cds.path <- getENSEMBL.Seq(organism, type = "cds", id.type = "all",
                        release = release, path = path)

        ensembl_download_post_processing(cds.path, organism,
                                         format = "cds",
                                         gunzip = gunzip,
                                         mute_citation = mute_citation)
    }
}








