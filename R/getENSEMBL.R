#' @title Helper function for retrieving biological sequence files from ENSEMBL
#' @description This function downloads gff files of query
#' organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type biological sequence type.
#' @param id.type a character, default "toplevel". id type of assembly, either "toplevel" or "primary_assembly" usually.
#' @param release a numeric, the database release version of ENSEMBL (\code{db = "ensembl"}). Default is \code{release = NULL} meaning
#' that the most recent database version is used. \code{release = 75} would for human would give the stable
#' GRCh37 release in ensembl. Value must be > 46, since ensembl did not structure their data
#' if the standard format before that.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @return either a character path to downloaded file, or a logical FALSE, specifying failure.
getENSEMBL.Seq <- function(organism, type = "dna", id.type = "toplevel", release = NULL, path) {

  getENSEMBL(organism, type, id.type, release, path, format = "fasta")
}

#' @title Helper function for retrieving gff/gtf files from ENSEMBL
#' @description This function downloads gff/gtff
#' files of query organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type specification type.
#' @param release the ENSEMBL release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBL.Annotation <- function(organism, type = "dna", release = NULL,
                                  path, format = "gff3") {
  getENSEMBL(organism, type, id.type = "none", release, path, format)
}

#' @title Helper function for retrieving gtf files from ENSEMBL
#' @description This function downloads gff
#' files of query organisms from ENSEMBL.
#' @inheritParams getENSEMBL.Seq
#' @import curl RCurl
#' @author Hajk-Georg Drost
#' @return character filepath to download file, returns FALSE if failed.
getENSEMBL.gtf <- function(organism, type = "dna",
                           path, release = NULL) {
  getENSEMBL.Annotation(organism, type, release, path, format = "gtf")[1]
}
