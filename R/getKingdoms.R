#' @title Retrieve available kingdoms of life stored in RefSeq
#' @description A short list of kingdoms of life that are stored in the RefSeq
#' database and that can be downloaded using e.g. \code{\link{meta.retrieval}}, \code{\link{getGenome}}, etc.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{refseq} or \code{genbank}.
#' Default is \code{db = "refseq"}.
#' @author Hajk-Georg Drost
#' @examples 
#' # retrieve kingdoms available at refseq
#' getKingdoms(db = "refseq")
#' 
#' # retrieve kingdoms available at genbank
#' getKingdoms(db = "genbank")
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}}
#' @export

getKingdoms <- function(db = "refseq"){
    
    if (db == "refseq") {
        return(
            c(
                "archaea",
                "bacteria",
                "fungi",
                "invertebrate",
                "plant",
                "protozoa",
                "vertebrate_mammalian",
                "vertebrate_other",
                "viral"
            )
        )
    }
    
    if (db == "genbank") {
        return(
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
        )
    }
}
