#' @title Retrieve All Available BioMart Databases
#' @description This funcion queries the BioMart API and returns a table
#' storing all available BioMart databases.
#' 
#' @author Hajk-Georg Drost
#' @examples
#' 
#' \dontrun{
#' # get a table of all available databases from BioMart
#'  getMarts()
#' }
#' 
#' @seealso \code{\link{getDatasets}}, \code{\link{getAttributes}}, \code{\link{getFilters}}, \code{\link{organismBM}}, \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export
getMarts <- function(){
    
    submarts <- c("ensembl", "plants", "fungi", "protists", "metazoa")
    submarts.df <- vector("list", length(submarts))
    for (i in seq_along(submarts)) {
        submarts.df[i] <- list(getSubMarts(submarts[i]))
    }
        
    return(dplyr::bind_rows(submarts.df))
}






