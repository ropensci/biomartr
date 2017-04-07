#' @title Perform Meta-Genome Retieval of all organisms in all kingdoms of life
#' @description Download genomes, proteomes, cds, gff, rna, or assembly stats 
#' files of individual species of all kingdoms of life.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved: 
#' \code{db = "refseq"}, \code{db = "genbank"}, \code{db = "emsembl"} 
#' or \code{db = "ensemblgenomes"}.
#' @param type type of sequences that shall be retrieved. Options are:
#' \itemize{
#'  \item \code{type = "genome"} :
#'  for genome assembly retrieval; see also \code{\link{getGenome}}), 
#'  \item \code{type = "proteome"} :
#'  (for proteome retrieval; see also \code{\link{getProteome}}),
#'  \item \code{type = "CDS"} :
#'  (for coding sequence retrieval; see also \code{\link{getCDS}}),
#'  \item \code{type = "gff"} :
#' (for annotation file retrieval in gff format; see also \code{\link{getGFF}}),
#'  \item \code{type = "rna"} :
#'  (for RNA file retrieval in fasta format; see also \code{\link{getRNA}}),
#'  \item \code{type = "assemblystats"} (for genome assembly quality stats 
#'  file retrieval; see also \code{\link{getAssemblyStats}}).
#'  }
#' @author Hajk-Georg Drost
#' @details This function aims to perform bulk retrieval of all genomes 
#' of species for all kingdoms of life.
#' @examples 
#' \dontrun{
#' # download all genomes from refseq
#' meta.retrieval.all(db = "refseq", type = "genome")
#' # download all vertebrate genomes from genbank
#' meta.retrieval.all(db = "genbank", type = "genome")
#' # download all vertebrate genomes from ensemblgenomes
#' meta.retrieval.all(db = "genbank", type = "ensemblgenomes")
#' }
#' @export

meta.retrieval.all <- function(db = "refseq", type = "genome") {
    # retrieve all genomes from all kingdoms of life
    lapply(getKingdoms(db = db), function(x) meta.retrieval(x, type = type, 
                                                            db = db, 
                                                            group = NULL))
}
