#' @title Retrieve a Collection: Genome, Proteome, CDS, RNA, GFF, Repeat Masker, AssemblyStats of multiple species
#' @description Main collection retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest a collection consisting of
#' the genome file, proteome file, CDS file, RNA file, GFF file, Repeat Masker file, AssemblyStats
#' file of the organism of interest
#' can be downloaded and stored locally. Collections can be retrieved from
#' several databases.
#' @param db a character string specifying the database from which the collection
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param organisms a character vector storing the scientific names of the organisms for
#' which collections shall be retrieved. There are three options to characterize an organism:
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param reference a logical value indicating whether or not a collection shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param release the database release version of ENSEMBL (\code{db = "ensembl"}). Default is \code{release = NULL} meaning that the most recent database version is used.
#' @param gunzip a logical value indicating whether or not files should be unzipped.
#' @param remove_annotation_outliers shall outlier lines be removed from the input annotation_file?
#'  If yes, then the initial annotation_file will be overwritten and the removed outlier lines
#'  will be stored at \code{\link{tempdir}} for further exploration.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding collection shall be stored. Default is
#' \code{path} = \code{file.path("_db_downloads","collections")}.
#' @param update a logical, default FALSE. The existing file will be retained if existing.
#' If TRUE, will download and overwrite the file.
#' @param clean_retrieval, a logical, default FALSE. Cleaning file names for more convenient
#'  downstream processing.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory '_ncbi_downloads/collection' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/collection' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded genome.
#' @examples \dontrun{
#' # define scientific names of species for which
#' # collections shall be retrieved
#' organism_list <- c("Arabidopsis thaliana",
#'                    "Arabidopsis lyrata",
#'                    "Capsella rubella")
#' # download the collection of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/collection'
#'  getCollectionSet( db       = "refseq",
#'              organism = organism_list,
#'              path = "set_collections")
#' }
#'
#' @seealso \code{\link{getCollection}}, \code{\link{getGenomeSet}}, \code{\link{getProteomeSet}}, \code{\link{getCDSSet}},
#' \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}},
#' \code{\link{read_genome}}
#' @export

getCollectionSet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           clean_retrieval = FALSE,
           gunzip = TRUE,
           update = FALSE,
           remove_annotation_outliers = TRUE,
           path = "set_collections") {
    message(
      "Starting collection retrieval of the following species: ",
      paste0(organisms, collapse = ", "),
      " ..."
    )

    if (!file.exists(path)) {
      message("Generating folder ", path, " ...")
      dir.create(path, recursive = TRUE)
    }

    if (!file.exists(file.path(path, "documentation")))
      dir.create(file.path(path, "documentation"))

    clean_names <- clean_species_names_helper(list.files(path), gunzip = gunzip)
    message("\n")
    if (length(clean_names) > 0) {
      for (j in seq_len(length(clean_names))) {
        if (file.exists(file.path(path, clean_names[j]))) {
          if (!update) {
            message("The file ", clean_names[j], " seems to exist already in ", path, ".",
                    "The existing file will be retained. Please specify 'update = TRUE' in case you wish to re-load the file.")
          }

          if (update) {
            message("The file ", clean_names[j], " seems to exist already in ", path, ".",
                    "You specified 'update = TRUE', thus the file ", clean_names[j], " will be downloaded again.")
            unlink(file.path(path, clean_names[j]), force = TRUE)
          }
        }

      }

    }

    if (!update && (length(organisms) > 1)) {
      organisms_short <- tidy_name2(organisms)
      clean_names_short <- unlist(sapply(clean_names, function(x) unlist(stringr::str_split(x, "[.]"))))
      organisms_short_setdiff <- as.character(as.vector(dplyr::setdiff(organisms_short, clean_names_short)))

      if (length(organisms) > 0) {
        organisms <- organisms[which(organisms_short %in% organisms_short_setdiff)]
      }
    }

    if (length(organisms) > 0) {
      paths <- vector("character", length(organisms))
      message("\n")

      for (i in seq_len(length(organisms))) {
        paths[i] <- getCollection(db       = db,
                              organism = organisms[i],
                              reference = reference,
                              gunzip = gunzip,
                              release = release,
                              remove_annotation_outliers = remove_annotation_outliers,
                              path     = path)
        message("\n")
      }

      meta_files <- list.files(path, db)
      meta_files <- meta_files[stringr::str_detect(meta_files, "doc_")]
      file.rename(from = file.path(path, db, meta_files), to = file.path(path, "documentation", meta_files))

      doc_tsv_files <- file.path(path,"documentation", meta_files[stringr::str_detect(meta_files, "[.]tsv")])

      summary_log <- dplyr::bind_rows(lapply(doc_tsv_files, function(data) {
        suppressMessages(readr::read_tsv(data))
      }))

      readr::write_excel_csv(summary_log, file.path(path, "documentation", paste0(basename(path), "_summary.csv")))
      message("A summary file (which can be used as supplementary information file in publications) containig retrieval information for all species has been stored at '",file.path(path, "documentation", paste0(basename(path), "_summary.csv")),"'.")

      if (clean_retrieval) {
        message("\n")
        message("Cleaning file names for more convenient downstream processing ...")
      }

      if (clean_retrieval && gunzip)
        clean.retrieval(paths, gunzip = FALSE)

      if (clean_retrieval && !gunzip)
        clean.retrieval(paths, gunzip = FALSE)

      # return file paths of clean names (if selected) and unzipped files (if selected)
      new_files <- list.files(path, db)

      return(file.path(path, db, new_files))
    } else {
      files <- file.path(path, db, list.files(file.path(path, db)))
      return(files)
    }
  }
