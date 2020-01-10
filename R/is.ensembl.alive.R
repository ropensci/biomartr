#' @title Helper function to test if ENSEMBL server is reachable
#' @description This function pings the ENSEMBL FTP server to 
#' test whether or not a connection can be established.
#' @author Hajk-Georg Drost
#' @noRd
is.ensembl.alive <- function() {
    ping <- jsonlite::fromJSON(
        "http://rest.ensembl.org/info/ping?content-type=application/json")
    if (ping$ping == 0)
        message("The ENSEMBL site seems to be out of order. Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall? Please try to go to 
             'http://rest.ensembl.org/' and see if the site opens properly.")
}
