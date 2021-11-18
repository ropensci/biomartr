#' @title Helper function to perform customized downloads
#' @description To achieve the most stable download experience,
#' ftp file downloads are customized for each operating system.
#' @param ... additional arguments that shall be passed to
#' \code{\link[downloader]{download}}
#' @author Hajk-Georg Drost
#' @noRd
custom_download <- function(url, ...) {

  withr::local_options(timeout = max(30000000, getOption("timeout")))
  
    if (RCurl::url.exists(url)) {
        operating_sys <- Sys.info()[1]

        if (operating_sys == "Darwin") {
            downloader::download(
                url = url, ...,
                cacheOK = FALSE,
                quiet = TRUE
            )

        }

        if (operating_sys == "Linux") {
            downloader::download(
                url = url, ...,
                cacheOK = FALSE,
                quiet = TRUE
            )
        }

        if (operating_sys == "Windows") {
            downloader::download(url = url, ...,
                                 method = "internal",
                                 cacheOK = FALSE,
                                 quiet = TRUE)
        }
    } else {
        message(
            "The FTP link: '",url,"' seems not to be available at the moment. This might be due to an instable internet connection, a firewall issue, or wrong organism name. Could you please try to re-run the function to see whether it works now?"
        )
        return(FALSE)
    }
}
