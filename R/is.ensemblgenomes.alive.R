#' @title Helper function to test if ENSEMBLGENOMES server is reachable
#' @description This function pings the ENSEMBLGENOMES FTP server to 
#' test whether or not a connection can be established.
#' @author Hajk-Georg Drost
#' @noRd
is.ensemblgenomes.alive <- function() {
    
    rest_api_status <- curl::curl_fetch_memory(
    "http://rest.ensembl.org/info/ping?content-type=application/json")$status_code
    
    if (rest_api_status != 200)
        message("The ENSEMBL site seems to be out of order. Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall? Please try to go to 
             'http://rest.ensembl.org/' and see if the site opens properly.")
}
