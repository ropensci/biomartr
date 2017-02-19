#' @title Retrieve available kingdoms of life
#' @description A short list of available kingdoms of life
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"}, \code{db = "ensembl"}, \code{db = "ensemblgenomes"}.
#' Default is \code{db = "refseq"}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' # retrieve kingdoms available from refseq
#' getKingdoms(db = "refseq")
#' 
#' # retrieve kingdoms available from genbank
#' getKingdoms(db = "genbank")
#' }
#' @seealso \code{\link{meta.retrieval}}, \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{getGroups}}
#' @export

getKingdoms <- function(db = "refseq"){
    
    if (!is.element(db, c("refseq", "genbank","ensembl", "ensemblgenomes")))
        stop("Please select one of the available data bases: 'refseq', 'genbank', 'ensembl', or 'ensemblgenomes'.", call. = FALSE)
    
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
    
    if (db == "ensembl") {
        ensemblinfo <- get.ensembl.info()
        ensemblgenomesinfo <-  get.ensemblgenome.info()
        
        joined.df <-
            dplyr::inner_join(ensemblinfo, ensemblgenomesinfo, by = "name")
        
        return(names(table(joined.df$division.x)))
    }
    
    if (db == "ensemblgenomes") {
        ensemblgenomesinfo <-  get.ensemblgenome.info()
        return(names(table(ensemblgenomesinfo$division)))
    }
}












