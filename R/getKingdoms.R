#' @title Retrieve available kingdoms of life
#' @description A short list of available kingdoms of life
#' @param db a character string specifying the database from which the genome
#' shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"},
#' \code{db = "ensembl"}, \code{db = "ensemblgenomes"}.
#' Default is \code{db = "refseq"}.
#' @author Hajk-Georg Drost
#' @examples
#' # retrieve kingdoms available from refseq
#' getKingdoms(db = "refseq")
#'
#' # retrieve kingdoms available from genbank
#' getKingdoms(db = "genbank")
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}},
#' \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{getGroups}}
#' @export

getKingdoms <- function(db = "refseq") {
  withr::local_options(timeout = max(30000000, getOption("timeout")))

    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop("Please select one of the available data bases: 'refseq',
             'genbank', 'ensembl', or 'ensemblgenomes'.", call. = FALSE)

  if (db %in% c("refseq", "genbank")) {
    return(getKingdomsNCBI(db))
  }

  return(getKingdomEnsembl())
}

getKingdomGenbank <- function() {
  c(
    "archaea",
    "bacteria",
    "fungi",
    "invertebrate",
    "plant",
    "protozoa",
    "vertebrate_mammalian",
    "vertebrate_other"
  )
}

getKingdomRefseq <- function() {
  c(getKingdomGenbank(), "viral")
}

getKingdomsNCBI <- function (db) {
  if (db == "refseq") {
    return(getKingdomRefseq())
  } else if (db == "genbank") {
    return(getKingdomGenbank())
  } else {
    stop("NCBI kingdoms only support 'db' as 'refseq' or 'genbank'.")
  }
}

getKingdomEnsembl <- function() {
  ensembl_divisions_short()
}








