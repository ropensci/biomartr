#' @title Helper function to perform customized downloads 
#' @description To achieve the most stable download experience, 
#' ftp file downloads are customized for each operating system.
#' @param ... additional arguments that shall be passed to
#' \code{\link[downloader]{download}}
#' @author Hajk-Georg Drost
#' @noRd
custom_download <- function(url, ...) {
    
    if (curl::curl_fetch_memory(url)$status_code == 226) {
        operating_sys <- Sys.info()[1]
        
        if (operating_sys == "Darwin") {
            downloader::download(
                url, ...,
                method = "curl",
                extra = "--retry 3",
                cacheOK = FALSE,
                quiet = TRUE
            )
            
        }
        
        if (operating_sys == "Linux") {
            downloader::download(
                url, ...,
                method = "wget",
                extra = "--tries 3 --continue",
                cacheOK = FALSE,
                quiet = TRUE
            )
        }
        
        if (operating_sys == "Windows") {
            downloader::download(url, ...,
                                 method = "internal",
                                 cacheOK = FALSE,
                                 quiet = TRUE)
        }
    } else {
        warning(
            "The FTP link: '",url,"' is not available. This might be due to an instable internet connection, a firewall issue, or wrong organism name.", call. = FALSE
        )
        return(FALSE)
    }
}
