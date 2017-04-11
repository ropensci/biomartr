#' @title Retrieve ENSEMBL info file
#' @description Retrieve species and genome information from 
#' http://rest.ensembl.org/info/species?content-type=application/json/.
#' @author Hajk-Georg Drost
#' @examples 
#' info.file <- getENSEMBLInfo()
#' info.file
#' @export 
getENSEMBLInfo <- function() {
    ENSEMBLInfoTable <- get.ensembl.info(update = TRUE)
    return(ENSEMBLInfoTable)
}
