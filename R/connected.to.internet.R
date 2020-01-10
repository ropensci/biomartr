#' @title Helper function to test if a stable internet connection 
#' can be established.
#' @description All retrieval functions need a stable
#' internet connection to work properly. This internal function pings 
#' the google homepage and throws an error if it cannot be reached.
#' @author Hajk-Georg Drost
#' @noRd
connected.to.internet <- function() {
    if (curl::curl_fetch_memory("www.google.com")$status_code == 200) {
        return(TRUE)
    } else {
        message(
            "It seems that you are not connected to the internet. A query to www.google.com was not successful. Could you please check?"
        )
    }
}

