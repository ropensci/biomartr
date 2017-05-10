#' @title Retrieve ENSEMBLGENOMES info file
#' @description Retrieve species and genome information from 
#' http://rest.ensemblgenomes.org/info/species?content-type=application/json/.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' info.file <- getENSEMBLGENOMESInfo()
#' info.file
#' }
#' @export 
getENSEMBLGENOMESInfo <- function() {
    ENSEMBLGENOMESInfoTable <- get.ensemblgenome.info(update = TRUE)
    return(ENSEMBLGENOMESInfoTable)
}
