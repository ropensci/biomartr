#' @title Helper function to test if ENSEMBLGENOMES server is reachable
#' @description This function pings the ENSEMBLGENOMES FTP server to 
#' test whether or not a connection can be established.
#' @author Hajk-Georg Drost
#' @noRd
is.ensemblgenomes.alive <- function() {
    
    rest_api_status <- curl::curl_fetch_memory(
    "http://rest.ensemblgenomes.org/info/ping?content-type=application/json")$status_code
    
    if (rest_api_status != 200)
        stop("The ENSEMBL site seems to be out of order or you might not be connected to the internet.",
             "Please try to go to 'http://rest.ensemblgenomes.org/' and see if the site opens properly.", call. = FALSE)
}
