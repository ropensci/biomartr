# All these will be removed


#' @title Annotation file retrieval from ENSEMBLGENOMES
#' @description Helper function for retrieving GFF files
#' of a particular organism of interest from the ENSEMBLGENOMES ftp server.
#' This function will be deprecated!
#' @param organism scientific name of the query organism
#' @param type type of biological sequence
#' @param id.type ENSEMBLGENOMES id type
#' @param release the ENSEMBLGENOMES release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param path where shall file be saved?
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.Annotation <- function(organism, type = "dna",
                                         id.type = "toplevel", release = NULL,
                                         path) {
  getENSEMBL.Annotation(organism, type, id.type, release, path)
}

#' This will be deprecated, use getENSEMBL.gtf.
#' @inheritParams getENSEMBL.gtf
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.gtf <- function(organism, type = "dna",
                                  id.type = "toplevel", path) {
  getENSEMBL.gtf(organism, type = "dna",
                 id.type = "toplevel", path, release)
}

#' @title Helper function for retrieving biological sequence files from
#' ENSEMBLGENOMES.
#' @description This function downloads gff files of query organisms from
#' ENSEMBLGENOMES. This will be deprecated, use getENSEMBL.Seq.
#' @inheritParams getENSEMBL.Seq
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.Seq <- function(organism, release = NULL, type = "dna",
                                  id.type = "toplevel", path) {
  getENSEMBL.Seq(organism, release, type, id.type, release, path)
}

#' @title Retrieve ENSEMBLGENOMES info file
#' @description Retrieve species and genome information from
#' http://rest.ensemblgenomes.org/info/species?content-type=application/json/.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' info.file <- getENSEMBLGENOMESInfo()
#' info.file
#' }
#' @export
getENSEMBLGENOMESInfo <- function() {
  ENSEMBLGENOMESInfoTable <- get.ensemblgenome.info(update = TRUE)
  return(ENSEMBLGENOMESInfoTable)
}
