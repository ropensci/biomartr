#' @title Retrieve information about available Ensembl Biomart databases 
#' @description This funcion queries the Ensembl Biomart API and returns a table
#' storing information about all available Ensembl Biomart databases.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # get a table of all available databases from Ensembl Biomart
#' getMarts()
#'  }
#' @seealso \code{\link{getDatasets}}, \code{\link{getAttributes}}, 
#' \code{\link{getFilters}}, \code{\link{organismBM}}, 
#' \code{\link{organismFilters}}, \code{\link{organismAttributes}}
#' @export
getMarts <- function(){
    
    submarts <- c("ensembl", "plants", "fungi", "protists", "metazoa")
    submarts.df <- vector("list", length(submarts))
    
    tryCatch({
    for (i in seq_along(submarts)) {
        submarts.df[i] <- list(getSubMarts(submarts[i]))
    }
        }, error = function(e)
        message(
            "It seems like the BioMart server could not be reached. This is either due to a server maintainence downtime or may be due to an instable internet connection on your side."
        ))
        
    return(dplyr::bind_rows(submarts.df))
}






