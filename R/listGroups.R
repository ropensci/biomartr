#' @title List number of available genomes in each group
#' @description Users can retrieve the available number of sequenced genomes per group. 
#' Only available for \code{db = "refseq"} and \code{db = "genbank"}.
#' @param db a character string specifying the database for which genome availability shall be checked, 
#' e.g. \code{db = "refseq"} and \code{db = "genbank"}. 
#' @author Hajk-Georg Drost
#' @examples 
#' \dontrun {
#' # example for refseq
#' listGroups(db = "refseq")
#' 
#' # example for genbank
#' listGroups(db = "genbank")
#' }
#' @seealso \code{\link{listGenomes}}, \code{\link{is.genome.available}}, \code{\link{listKingdoms}}, \code{\link{listSubgroups}}
#' @export

listGroups <- function(db = "refseq") {
    
    if (!is.element(db, c("refseq", "genbank")))
        stop("Unfortunately, only db = 'refseq' and db = 'genbank' provide group information.")
    
    organism_name <- group <- NULL
    
    listgenomes.data <-
        listGenomes(db = db, type = "group", details = TRUE)
    
    uniq.species <-
        dplyr::summarise(
            dplyr::group_by(listgenomes.data, organism_name, group),
            unique_elements = dplyr::n_distinct(organism_name)
        )
    
    return(table(uniq.species$group))
}
