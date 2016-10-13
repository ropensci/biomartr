#' @title List available metagenomes on NCBI Genbank
#' @description List available metagenomes on NCBI genbank. NCBI genbank allows users
#' to download entire metagenomes of several metagenome projects. This function lists
#' all available metagenomes that can then be downloaded via \code{\link{getMetaGenomes}}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' listMetaGenomes()
#' } 
#' @seealso \code{\link{getMetaGenomes}}, \code{\link{getMetaGenomeSummary}}
#' @export
#' 
listMetaGenomes <- function() {

    metagenome.summary <- getMetaGenomeSummary()    
    return(unique(metagenome.summary$organism_name))
}
