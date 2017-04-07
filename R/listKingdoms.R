#' @title List number of available genomes in each kingdom of life
#' @description Users can retrieve the available number of sequenced 
#' genomes per kingdom.
#' @param db a character string specifying the database for which genome 
#' availability shall be checked, 
#' e.g. \code{db = "refseq"}, \code{db = "genbank"}, \code{db = "ensembl"}, 
#' \code{db = "ensemblgenomes"}. 
#' @author Hajk-Georg Drost
#' @examples 
#' \dontrun{
#' # list number of available genomes in refseq for each kingdom of life
#' listKingdoms(db = "refseq")
#' # example for genbank
#' listKingdoms(db = "genbank")
#' # example for ensembl
#' listKingdoms(db = "ensembl")
#' # example for ensemblgenomes
#' listKingdoms(db = "ensemblgenomes")
#' }
#' @seealso \code{\link{listGenomes}}, \code{\link{is.genome.available}}, 
#' \code{\link{listGroups}}
#' @export

listKingdoms <- function(db = "refseq") {
    organism_name <- kingdoms <- name <- division <- NULL
    
    listgenomes.data <-
        listGenomes(db = db,
                    type = "kingdom",
                    details = TRUE)
    
    if (is.element(db, c("refseq", "genbank"))) {
        uniq.species <-
            dplyr::summarise(
                dplyr::group_by(listgenomes.data, organism_name, kingdoms),
                unique_elements = dplyr::n_distinct(organism_name)
            )
        
        return(table(uniq.species$kingdoms))
    }
    
    if (is.element(db, c("ensembl", "ensemblgenomes"))) {
        uniq.species <-
            dplyr::summarise(
                dplyr::group_by(listgenomes.data, name, division),
                unique_elements = dplyr::n_distinct(name)
            )
        
        return(table(uniq.species$division))
    }
}
