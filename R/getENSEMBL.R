#' @title Helper function for retrieving biological sequence files from ENSEMBL
#' @description This function downloads gff files of query
#' organisms from ENSEMBL.
#' @inheritParams getENSEMBL
#' @author Hajk-Georg Drost
#' @return either a character path to downloaded file, or a logical FALSE, specifying failure.
getENSEMBL.Seq <- function(organism, type = "dna", id.type = "toplevel", release = NULL, path) {

  getENSEMBL(organism, type, id.type, release, path, format = "fasta")
}

#' @title Helper function for retrieving gff/gtf files from ENSEMBL
#' @description This function downloads gff/gtff
#' files of query organisms from ENSEMBL.
#' @inheritParams getENSEMBL
#' @author Hajk-Georg Drost
#' @noRd
getENSEMBL.Annotation <- function(organism, type = "dna", release = NULL,
                                  path, format = "gff3") {
  getENSEMBL(organism, type, id.type = "none", release, path, format)
}

#' @title Helper function for retrieving gtf files from ENSEMBL
#' @description This function downloads gff
#' files of query organisms from ENSEMBL.
#' @inheritParams getENSEMBL
#' @import curl RCurl
#' @author Hajk-Georg Drost
#' @return character filepath to download file, returns FALSE if failed.
getENSEMBL.gtf <- function(organism, type = "dna",
                           path, release = NULL) {
  getENSEMBL.Annotation(organism, type, release, path, format = "gtf")[1]
}
