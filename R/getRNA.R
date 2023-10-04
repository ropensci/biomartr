#' @inherit getBio
#' @title RNA Sequence Retrieval
#' @description Main retrieval function for RNA sequences of an organism
#' of interest. By specifying the scientific name of an organism of interest the
#' corresponding fasta-file storing the RNA information for the organism
#' of interest can be downloaded and stored locally.
#' RNA files can be retrieved from several databases.
#'
#' @param path a character string specifying the location (a folder) in which
#' the corresponding
#' CDS file shall be stored. Default is
#' \code{path} = \code{file.path("_ncbi_downloads","RNA")}.
#' @examples
#' \dontrun{
#' # download the RNA of Arabidopsis thaliana from refseq
#' # and store the corresponding RNA file in '_ncbi_downloads/RNA'
#' file_path <- getRNA( db       = "refseq",
#'              organism = "Arabidopsis thaliana",
#'              path     = file.path("_ncbi_downloads","RNA"))
#'
#' Ath_RNA <- read_rna(file_path, format = "fasta")
#' }
#' @family getBio
#' @family rna
#' @export
getRNA <-
    function(db = "refseq",
             organism,
             reference = FALSE,
             skip_bacteria = TRUE,
             release = NULL,
             assembly_type = "toplevel",
             path = file.path("_ncbi_downloads", "RNA"),
             gunzip = FALSE,
             mute_citation = FALSE) {


      if (!is.element(db, c("refseq", "genbank", "ensembl")))
          stop(
              "Please select one of the available data bases:
              'refseq', 'genbank', or 'ensembl'.",
              call. = FALSE
          )

      if (db == "ensemblgenomes") {
              organism_name <- is.genome.available(db = db, organism = organism, details = TRUE)$display_name[1]

              if (!is.na(organism_name))
                      message("-> Starting RNA retrieval of '", organism_name, "' from ", db, " ...")
              if (is.na(organism_name))
                      message("-> Starting RNA retrieval of '", organism, "' from ", db, " ...")
      } else {
          message("-> Starting RNA retrieval of '", organism, "' from ", db, " ...")
          message("\n")
      }

      if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
      }

      if (is.element(db, c("refseq", "genbank"))) {
        info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                        release, gunzip, path, assembly_type,
                                        type = "rna")
        refseq_genbank_download_post_processing(info, organism, db, path,
                                                gunzip,
                                                remove_annotation_outliers = FALSE,
                                                format = "rna",
                                                mute_citation = mute_citation)
      } else if (db %in% c("ensembl", "ensemblgenomes")) {
          rna.path <- getENSEMBL.Seq(
                          organism,
                          type = "ncrna",
                          id.type = "none",
                          release = release,
                          path = path
                  )

          ensembl_download_post_processing(rna.path, organism,
                                           format = "ncrna",
                                           gunzip = gunzip,
                                           mute_citation = mute_citation)
      }
  }








