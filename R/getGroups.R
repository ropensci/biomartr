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
#' # retrieve subgroups for vertebrate_mammalian available at refseq
#' getGroups(kingdom = "vertebrate_mammalian", db = "refseq")
#' 
#' # get possible kigdom names
#' getKingdoms(db = "genbank")
#' # retrieve subgroups for vertebrate_mammalian available at genbank
#' getGroups(kingdom = "vertebrate_mammalian",db = "genbank")
#' }
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{getKingdoms}}, \code{\link{getSubgroups}}
#' @export

getGroups <- function(kingdom, db = "refseq") {
    
    if (!is.element(kingdom, getKingdoms(db = db)))
        stop(paste0(
            "Please select a valid kingdom: ",
            paste0(getKingdoms(db = db), collapse = ", ")
        ), call. = FALSE)
    
    if (!is.element(db, c("refseq", "genbank")))
        stop("Please select one of the available data bases: 'refseq', 'genbank'.", call. = FALSE)
    
    # get Kingdom Assembly Summary file
    AssemblyFilesAllKingdoms <- getKingdomAssemblySummary(db = db)
    
    # get all genomes list
    all.genomes <- listGenomes(db = db, type = "group", details = TRUE)
    
    # join tables
    joined.df <- dplyr::inner_join(AssemblyFilesAllKingdoms,all.genomes, by = "organism_name")
    
    return(names(table(joined.df$group)))
}
