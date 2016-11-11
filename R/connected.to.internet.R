connected.to.internet <- function() {
    if (is.character(RCurl::getURL("www.google.com"))){
        TRUE
    } else {
        stop("It seems that you are not connected to the internet. Could you please check?", call. = FALSE)
    }
}

