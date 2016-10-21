is.ensembl.alive <- function() {
    
    ping <- jsonlite::fromJSON("http://rest.ensembl.org/info/ping?content-type=application/json")
    if (ping$ping == 0)
        stop("The ENSEMBL site seems to be out of order or you might not be connected to the internet. Please try to go to 'http://rest.ensembl.org/' and see if the site opens properly.")
}
