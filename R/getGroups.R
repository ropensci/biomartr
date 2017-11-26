#' @title Retrieve available groups for a kingdom of life
#' @description A short list of available groups for a kingdom of life.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved: 
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' }
#' 
#' Default is \code{db = "refseq"}.
#' @param kingdom a character string specifying for which kingdom of life 
#' groups shall be retrieved. See \code{\link{getKingdoms}} for details.
#' @author Hajk-Georg Drost
#' @examples
#' # get possible kigdom names
#' getKingdoms(db = "refseq")
#' \dontrun{
#' # retrieve subgroups for vertebrate_mammalian available from refseq
#' getGroups(db = "refseq", kingdom = "vertebrate_mammalian")
#' 
#' # get possible kigdom names
#' getKingdoms(db = "genbank")
#' # retrieve subgroups for vertebrate_mammalian available from genbank
#' getGroups(db = "genbank", kingdom = "vertebrate_mammalian")
#' }
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}}, 
#' \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{getKingdoms}}
#' @export

getGroups <- function(db = "refseq", kingdom) {
    groups <- listGroups(db = db, kingdom = kingdom, details = FALSE)
    return(names(groups))
}
