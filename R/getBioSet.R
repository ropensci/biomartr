#' Generic Bio data set extractor
#'
#' Usually you want to use one of the specific set extractors
#' @inheritParams getBio
#' @param organisms a character vector storing the names of the organisms than shall be retrieved.
#' There are three available options to characterize an organism:
#' @param path character, default location is paste0("set_", toupper(set_type))
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory 'set_CDSs' to store
#' the CDSs of interest as fasta files for future processing.
#' In case the corresponding fasta file already exists within the
#' 'set_CDSs' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded genomes
#' (names are identifiers: 'new' (file was downloaded now),
#' 'old' files did already exist)
#' @examples \dontrun{
#' getBioSet("refseq", organisms = c("Arabidopsis thaliana",
#'                                   "Arabidopsis lyrata",
#'                                   "Capsella rubella"), set_type = "cds")
#' }
#' @family getBioSet
#' @export
getBioSet <- function(db = "refseq",
                      organisms,
                      set_type,
                      reference = FALSE,
                      release = NULL,
                      gunzip = TRUE,
                      update = FALSE,
                      skip_bacteria = TRUE,
                      path = paste0("set_", toupper(set_type)),
                      remove_annotation_outliers = FALSE,
                      assembly_type = "toplevel",
                      format = "gff3",
                      mute_citation = FALSE) {
  stopifnot(length(organisms) > 0)
  set_upper <- toupper(set_type)
  message(
    "Starting ", set_upper, " retrieval of the following ", set_upper, "s: ",
    paste0(organisms, collapse = ", "),
    " ..."
  )

  if (!dir.exists(path)) {
    message("Generating folder ", path, " ...")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(file.path(path, "documentation")))
    dir.create(file.path(path, "documentation"), showWarnings = FALSE)

  message("\n")
  organisms <- check_if_tidy_exists(organisms, path, update, set_type)
  to_run <- names(organisms) == "new"
  if (any(to_run)) {
    message("\n")
    paths <- c()
    for (organism in organisms[to_run]) {
      out <-  getBio(db, organism, type = set_type, reference = reference,
                     release = release, gunzip = gunzip, update = update,
                     skip_bacteria = skip_bacteria, path = path,
                     remove_annotation_outliers = remove_annotation_outliers,
                     assembly_type = assembly_type, format = format,
                     mute_citation = TRUE)
      paths <- c(paths, out)
      message("\n")
    }
    make_summary_doc_set(path)
    mark_as_complete_set(organisms, path, paths, set_type)
  }

  dt <- data.table::fread(done_organisms_bioset_file(path, set_type))
  paths <- dt[data.table::chmatch(organism, tidy_name2(organisms)),]$file
  names(paths) <- names(organisms)

  please_cite_biomartr(mute_citation = mute_citation)
  return(paths)
}

#' A wrapper to all bio getters, selected with 'type' argument
#'
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param organism Organism selector id,
#' there are three options to characterize an organism:
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param type biological sequence type. (alternatives are: genome, gff, cds, rna, proteome, collection (all the others))
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param release a numeric, the database release version of ENSEMBL (\code{db = "ensembl"}). Default is \code{release = NULL} meaning
#' that the most recent database version is used. \code{release = 75} would for human would give the stable
#' GRCh37 release in ensembl. Value must be > 46, since ensembl did not structure their data
#' if the standard format before that.
#' @param gunzip a logical, indicating whether or not files should be unzipped.
#' @param update logical, default FALSE. Updated backend cached files needed.
#' Usually keep this false, to make ut run much faster. Only set to TRUE, if you
#' believe you cache is outdated (Species only exist in newest release etc)
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @param path character, default location is paste0("set_", toupper(type))
#' @param remove_annotation_outliers shall outlier lines be removed from the input \code{annotation_file}?
#' If yes, then the initial \code{annotation_file} will be overwritten and the removed outlier lines will be stored at \code{\link{tempdir}}
#' for further exploration.
#' @param analyse_genome logical, default FALSE. If TRUE, get general genome statistics like
#' gc content etc. For more details, see ?summary_genome
#' @param assembly_type a character, default "toplevel". id type of assembly, either "toplevel" or "primary_assembly" usually.
#' @param format "gff3", alternative "gtf" for ensembl.
#' @param mute_citation logical, default FALSE, indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory relative to file type, if you get fasta genomes it will be
#' _ncbi_downloads/genomes'.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomes' folder and is accessible within the workspace,
#' no download process will be performed.
#' For other file types the same rule applies.
#' @family getBio
#' @return File path to downloaded genome.
getBio <- function(db = "refseq",
                   organism,
                   type,
                   reference = FALSE,
                   release = NULL,
                   gunzip = FALSE,
                   update = FALSE,
                   skip_bacteria = TRUE,
                   path = paste0("set_", toupper(type)),
                   remove_annotation_outliers = FALSE,
                   analyse_genome = FALSE,
                   assembly_type = "toplevel",
                   format = "gff3",
                   mute_citation = FALSE) {
  if (type == "genome") {
    getGenome(db, organism, reference = reference,
              skip_bacteria = skip_bacteria, release = release,
              gunzip = gunzip, path = path, assembly_type = assembly_type,
              mute_citation = mute_citation, analyse_genome = analyse_genome)
  } else if (type == "gff") {
    getGFF(db, organism, reference = reference,
           skip_bacteria = skip_bacteria, release = release,
           gunzip = gunzip,
           remove_annotation_outliers = remove_annotation_outliers,
           path = path,
           mute_citation = mute_citation, format = format)
  } else if (type == "cds") {
    getCDS(db, organism, reference = reference,
           skip_bacteria = skip_bacteria,
           release = release,
           gunzip = gunzip,
           path = path,
           mute_citation = mute_citation)
  } else if (type == "rna") {
    getRNA(db, organism, reference = reference,
           skip_bacteria = skip_bacteria,
           release = release,
           gunzip = gunzip,
           path = path,
           mute_citation = mute_citation)
  } else if (type %in% c("protein", "proteome")) {
    getProteome(db, organism, reference = reference,
                skip_bacteria = skip_bacteria,
                release = release,
                gunzip = gunzip,
                path = path,
                mute_citation = mute_citation)
  } else if (type == "collection") {
    getCollection(db, organism, reference = reference,
                  skip_bacteria = skip_bacteria,
                  release = release,
                  gunzip = gunzip,
                  remove_annotation_outliers = FALSE,
                  path = path,
                  mute_citation = mute_citation)
  } else stop("invalid bio getter type selected")
}

make_summary_doc_set <- function(path, out = file.path(path, "documentation", paste0(basename(path), "_summary.csv"))) {
  meta_files <- list.files(path)
  meta_files <- meta_files[stringr::str_detect(meta_files, "doc_")]
  if (length(meta_files) == 0) return(invisible(NULL))

  file.rename(from = file.path(path, meta_files), to = file.path(path, "documentation", meta_files))

  doc_tsv_files <- file.path(path,"documentation", meta_files[stringr::str_detect(meta_files, "[.]tsv")])

  summary_log <- data.table::rbindlist(lapply(doc_tsv_files, function(data) {
    suppressMessages(data.table::fread(data))
  }))

  data.table::fwrite(summary_log, out)
  message("A summary file (which can be used as supplementary information file in publications)",
  " containig retrieval information for all species has been stored at '", out, "'.")
  return(invisible(NULL))
}

check_if_tidy_exists <- function(organisms, path, update, set_type) {
  # Load done csv if existing
  done_org_list <- done_organisms_bioset_file(path, set_type)
  names(organisms) <- rep("new", length(organisms))
  if (file.exists(done_org_list)) {
    done_org_list <- data.table::fread(done_org_list)

    organisms_short <- tidy_name2(organisms)
    # If update, remove all
    done_orgs <- done_org_list$organism %in% organisms_short
    if (any(done_orgs)) {
      if (update) {
        file.remove(done_org_list[done_orgs,]$file)
      } else {
        match <- organisms_short %in% done_org_list$organism

        names(organisms)[match] <- "exist"
      }
    }
    if (all(done_orgs) & !update) {
      message("- Skipping computation of set (all species already done),",
              " set update to TRUE if you want to run again")
    }
  }

  return(organisms)
}

mark_as_complete_set <- function(organisms, path, new_files, set_type) {
  # Load done csv if existing
  stopifnot(names(organisms) %in% c("new", "exist"))

  # If update, remove all
  to_be_appended <- names(organisms) == "new"
  if (any(to_be_appended)) {
    done_org_list_file <- done_organisms_bioset_file(path, set_type)
    if (file.exists(done_org_list_file)) {
      done_org_list <- data.table::fread(done_org_list)
    } else done_org_list <- data.table::data.table()
    organisms_short <- tidy_name2(organisms)
    append <- data.table::data.table(organism = organisms_short[to_be_appended],
                                     file = new_files[to_be_appended])
    new_list <- data.table::rbindlist(list(done_org_list, append))
    data.table::fwrite(new_list, done_org_list_file)
    message("- List of completed organisms updated in: ", done_org_list_file)
  }
  return(invisible(NULL))
}

done_organisms_bioset_file <- function(path, set_type) {
  file.path(path, paste0(set_type, "_done_organisms.csv"))
}
