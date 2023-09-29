#' @title Proteome Retrieval
#' @description Main proteome retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest the
#' corresponding fasta-file storing the proteome of the organism of interest
#' can be downloaded and stored locally. Proteome files can be retrieved from
#' several databases.
#' @inheritParams getGenome
#' @param path a character string specifying the location (a folder) in which
#' the corresponding proteome shall be stored. Default is
#' \code{path} = \code{file.path("_ncbi_downloads","proteomes")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory '_ncbi_downloads/proteomes' to store
#' the proteome of interest as fasta file for future processing.
#' @return File path to downloaded proteome.
#' @examples \dontrun{
#'
#' # download the proteome of Arabidopsis thaliana from refseq
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' file_path <- getProteome( db       = "refseq",
#'              organism = "Arabidopsis thaliana",
#'              path     = file.path("_ncbi_downloads","proteomes") )
#'
#' Ath_proteome <- read_proteome(file_path, format = "fasta")
#'
#' # download the proteome of Arabidopsis thaliana from genbank
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' file_path <- getProteome( db       = "genbank",
#'              organism = "Arabidopsis thaliana",
#'              path     = file.path("_ncbi_downloads","proteomes") )
#'
#' Ath_proteome <- read_proteome(file_path, format = "fasta")
#' }
#' @seealso \code{\link{getGenome}}, \code{\link{getCDS}}, \code{\link{getGFF}},
#' \code{\link{getRNA}}, \code{\link{getRepeatMasker}},
#' \code{\link{getAssemblyStats}}, \code{\link{getCollection}}, \code{\link{meta.retrieval}},
#' \code{\link{read_proteome}}
#' @export

getProteome <-
    function(db = "refseq",
             organism,
             reference = TRUE,
             skip_bacteria = TRUE,
             release = NULL,
             gunzip = FALSE,
             path = file.path("_ncbi_downloads", "proteomes"),
             mute_citation = FALSE) {
  if (!is.element(db, c("refseq", "genbank",
                        "ensembl", "ensemblgenomes", "uniprot")))
      stop(
          "Please select one of the available data bases:
          'refseq', 'genbank', 'ensembl', 'ensemblgenomes' or 'uniprot'.",
          call. = FALSE
      )

  # create result folder
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  if (db == "ensemblgenomes") {
          organism_name <- is.genome.available(db = db, organism = organism, details = TRUE)$display_name[1]

          if (!is.na(organism_name))
                  message("Starting proteome retrieval of '", organism_name, "' from ", db, " ...")
          if (is.na(organism_name))
                  message("Starting proteome retrieval of '", organism, "' from ", db, " ...")

          message("\n")
  } else {
      message("-> Starting proteome retrieval of '", organism, "' from ", db, " ...")
      message("\n")
  }

    if (is.element(db, c("refseq", "genbank"))) {
      info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                      release, gunzip, path, type = "protein")
      refseq_genbank_download_post_processing(info, organism, db, path,
                                              gunzip,
                                              remove_annotation_outliers = FALSE,
                                              format = "protein",
                                              mute_citation = mute_citation)
    } else if (db %in% c("ensembl", "ensemblgenomes")) {
        proteome.path <- getENSEMBL.Seq(organism, type = "pep",
                                        id.type = "all",
                                        release = release,
                                        path = path)
        return(ensembl_download_post_processing(proteome.path, organism,
                                                format = "pep.fa",
                                                remove_annotation_outliers = FALSE,
                                                gunzip,
                                                mute_citation = mute_citation))


    } else if (db == "uniprot") {
        getUniProtSeq(organism = organism, path = path, update = TRUE,
                      gunzip = gunzip)
        please_cite_biomartr(mute_citation = mute_citation)
    }
}






