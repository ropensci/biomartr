
clean.str.brackets <- function(string) {

    str.new <- stringr::str_replace(string,"\\(","\\\\\\\\(")
    str.new <- stringr::str_replace(str.new,"\\)","\\\\\\\\)")

    return(str.new)
}

is.taxid <- function(x) {
  return(stringr::str_count(x, "[:digit:]") == nchar(x))
}

setTMPFile <- function(file.name){

        tempfile(pattern = file.name, tmpdir = tempdir(), fileext = "")
}


getTMPFile <- function(file.name){

        file.path(tempdir(),file.name)

}

#' Gunzip and checks post download
#'
#' @noRd
gunzip_and_check <- function(file, gunzip = FALSE,
                             remove_annotation_outliers = FALSE, format,
                             mute_citation = FALSE) {
  final_path <- file
  if (gunzip) {
    final_path <- unlist(stringr::str_replace(final_path, "[.]gz", ""))
  }

  message("-> The ", toupper(format), " has been downloaded to '",
          dirname(final_path), "' and has been named '",
          basename(final_path), "'."
  )

  if (gunzip) {
    message("-> Unzipping downloaded file ...")
    R.utils::gunzip(file, destname = final_path)
  }
  if (format == "gff3") {
    output_path <- check_annotation_biomartr(final_path, remove_annotation_outliers)
  }


  please_cite_biomartr(mute_citation = mute_citation)
  return(final_path)
}

#' Validates md5 sum test given url and local file
#' @param md5_local the path to download md5 file
#' @param md5_url the md5 url to download
#' @param local_file the existing local file to check
#' @param file_url full path to online file,
#' the file named paste0("./", basename(file_url)) must exist in md5 name list.
#' @noRd
md5_sum_test <- function(md5_local, md5_url, local_file, file_url) {
  if (!file.exists(local_file))
    stop("The local file: ", local_file, " does not exist to check md5 sum of!")
  custom_download(
    md5_url,
    md5_local,
    mode = "wb"
  )
  # test check sum
  md5_file_path <- md5_local
  md5_file <- read_md5file(md5_file_path)

  file_name <- NULL

  md5_sum <- dplyr::filter(md5_file,
                           file_name == paste0("./", basename(file_url)))$md5

  message("-> Checking md5 hash of file: ",
          local_file , " (md5: ", md5_sum, ")", " ...")

  if (!(tools::md5sum(local_file) == md5_sum))
    stop(
      paste0(
        "Please download the file '",
        md5_file_path,
        "' again. The md5 hash between the downloaded file and the file ",
        "stored at NCBI do not match.",
        collapse = ""
      )
    )
  unlink(md5_file_path)
  message("-> The md5 hash of file '", md5_file_path, "' matches!")
  return(invisible(NULL))
}

test <- function(x){ print(paste0("Test ",x," passed.","\n"))}

#' Get directory to store back end files like kingdom summaries etc
#' @param non_temp_cache "~/.biomartr_cache_dir.rds",
#' @return reads the rds file, and returns the path for local cache, if not
#' existing, use tempdir().
#' @export
#' @examples
#' cachedir()
#' @family cachedir
cachedir <- function(non_temp_cache = "~/.biomartr_cache_dir.rds") {
  if (file.exists(non_temp_cache)) {
    readRDS(non_temp_cache)
  } else tempdir()
}

#' Set directory to store back end files like kingdom summaries etc
#' @param path the path to cache dir, example "~/Bio_data/biomartr_cache/"
#' @return invisible(NULL), only save the file to path location
#' @export
#' @examples
#' # By default it is tempdir()
#' cachedir()
#' # cachedir_set("~/Bio_data/biomartr_cache/")
#' cachedir()
#' @family cachedir
cachedir_set <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  saveRDS(path, file = "~/.biomartr_cache_dir.rds")
}

db_hosts <- function() {
  c(
    "refseq",
    "genbank",
    "ensembl",
    "ensemblgenomes",
    "uniprot"
  )
}

supported_biotypes <- function(db) {
  # TODO: maybe implement for uniprot ?
  bioTypes <- c("genome", "proteome", "cds", "rna")
  names(bioTypes) <- rep("fasta", length(bioTypes))
  bioTypes <- c(bioTypes, "gff3" = "gff")
  if (is.element(db, c("refseq", "genbank"))) {
    bioTypes <- c(bioTypes, c(fasta = "repeat_masker", txt = "assembly_stats"))
  } else if (is.element(db, c("ensembl", "ensemblgenomes"))) {
    bioTypes <- c(bioTypes, c(gtf = "gff"))
  }
  return(bioTypes)
}
