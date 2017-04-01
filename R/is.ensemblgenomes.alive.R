#' @title Helper function to test if ENSEMBLGENOMES server is reachable
#' @description This function pings the ENSEMBLGENOMES FTP server to 
#' test whether or not a connection can be established.
#' @author Hajk-Georg Drost
#' @noRd
is.ensemblgenomes.alive <- function() {
    
    ping <- jsonlite::fromJSON("http://rest.ensemblgenomes.org/info/ping?content-type=application/json")
    if (ping$ping == 0)
        stop("The ENSEMBL site seems to be out of order or you might not be connected to the internet. Please try to go to 'http://rest.ensembl.org/' and see if the site opens properly.")
}
