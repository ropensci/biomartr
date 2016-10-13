#' @title Retrieve metagenomes from NCBI Genbank
#' @description Retrieve available metagenomes from NCBI Genbank. NCBI Genbank allows users
#' to download entire metagenomes of several metagenome projects. This function downloads
#'  available metagenomes that can then be downloaded via \code{\link{getMetaGenomes}}.
#' @param name metagenome name retrieved by \code{\link{listMetaGenomes}}.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' metagenome shall be stored. Default is \code{path} = \code{file.path("_ncbi_downloads","metagenome")}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # Frist, retrieve a list of available metagenomes
#' listMetaGenomes()
#' 
#' # Now, retrieve the 'human gut metagenome'
#' getMetaGenomes(name = "human gut metagenome")
#' } 
getMetaGenomes <- function(name, path = file.path("_ncbi_downloads","metagenome")) {
    
    if (!is.element(name, listMetaGenomes()))
        stop("Unfortunately, the metagenome '",name,"' is not available. Please consult the listMetaGenomes() function for available metagenomes.")
    
    
}
