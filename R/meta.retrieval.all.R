#' @title Perform Meta-Genome Retieval of all organisms in all kingdoms of life
#' @description Download genomes, proteomes, or CDS of individual species of all kingdoms of life.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{refseq} or \code{genbank}.
#' @param type type of sequences that shall be retrieved. Either \code{genome}, \code{proteome}, or \code{CDS}.
#' @author Hajk-Georg Drost
#' @details This function aims to perform bulk retrieval of all genomes of species
#' for all kingdoms of life.
#' @examples 
#' \dontrun{
#' # download all genomes from refseq
#' meta.retrieval.all(db = "refseq", type = "genome")
#' 
#' # download all vertebrate genomes from genbank
#' meta.retrieval.all(db = "genbank", type = "genome")
#' 
#' # download all vertebrate genomes from ensemblgenomes
#' meta.retrieval.all(db = "genbank", type = "ensemblgenomes")
#' }
#' @export

meta.retrieval.all <- function(db = "refseq", type = "genome") {
    # retrieve all genomes from all kingdoms of life
    sapply(getKingdoms(db = db), function(x) meta.retrieval(x, type = type, db = db))
    
}
