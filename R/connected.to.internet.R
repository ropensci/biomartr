#' @title Helper function to test if a stable internet connection 
#' can be established.
#' @description All retrieval functions need a stable
#' internet connection to work properly. This internal function pings 
#' the google homepage and throws an error if it cannot be reached.
#' @author Hajk-Georg Drost
#' @noRd
connected.to.internet <- function() {
    if (is.character(RCurl::getURL("www.google.com"))) {
        TRUE
    } else {
        stop(
            "It seems that you are not connected to the internet.
            Could you please check?",
            call. = FALSE
        )
    }
}

