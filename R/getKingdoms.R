#' @title Retrieve available kingdoms of life stored in RefSeq
#' @description A short list of kingdoms of life that are stored in the RefSeq
#' database and that can be downloaded using e.g. \code{\link{meta.retrieval}}, \code{\link{getGenome}}, etc.
#' @author Hajk-Georg Drost
#' @examples 
#' getKingdoms()
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}}
#' @export

getKingdoms <- function(){
        return( c("archaea","bacteria", "fungi", "invertebrate", "plant",
          "protozoa", "vertebrate_mammalian", "vertebrate_other") )
}