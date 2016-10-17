#' @title List available metagenomes on NCBI Genbank
#' @description List available metagenomes on NCBI genbank. NCBI genbank allows users
#' to download entire metagenomes of several metagenome projects. This function lists
#' all available metagenomes that can then be downloaded via \code{\link{getMetaGenomes}}.
#' @param details a boolean value specifying whether only the scientific names of stored metagenomes shall be returned
#' (\code{details = FALSE}) or all information such as "organism_name","bioproject", etc (\code{details = TRUE}).
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # retrieve available metagenome projects at NCBI Genbank
#' listMetaGenomes()
#' 
#' # retrieve detailed information on available metagenome projects at NCBI Genbank
#' listMetaGenomes(details = TRUE)
#' } 
#' @seealso \code{\link{getMetaGenomes}}, \code{\link{getMetaGenomeSummary}}
#' @export
#' 
listMetaGenomes <- function(details = FALSE) {

    metagenome.summary <- getMetaGenomeSummary()    
    
    if (!details)
        return(unique(metagenome.summary$organism_name))
    
    if (details)
        return(metagenome.summary)
}
