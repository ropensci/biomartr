#' @title Retrieve available groups for a kingdom of life
#' @description A short list of available groups for a kingdom of life.
#' @param kingdom a character string specifying for which kingdom of life groups shall be retrieved. See \code{\link{getKingdoms}} for details.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"}.
#' Default is \code{db = "refseq"}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' # get possible kigdom names
#' getKingdoms(db = "refseq")
#' # retrieve subgroups for vertebrate_mammalian available from refseq
#' getGroups(kingdom = "vertebrate_mammalian", db = "refseq")
#' 
#' # get possible kigdom names
#' getKingdoms(db = "genbank")
#' # retrieve subgroups for vertebrate_mammalian available from genbank
#' getGroups(kingdom = "vertebrate_mammalian",db = "genbank")
#' }
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{getKingdoms}}
#' @export

getGroups <- function(kingdom, db = "refseq") {
    groups <- listGroups(db = db, kingdom = kingdom, details = FALSE)
    return(names(groups))
}
