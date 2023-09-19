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
                                 cacheOK = FALSE,
                                 quiet = TRUE)
        }
    } else {
        message(
            "The FTP link: '",url,"' seems not to be available at the moment. This might either be due to an instable internet connection, a firewall issue, a wrong organism name, or due to the fact that the specified organism is not available in the database you selected. If it is an internet connection issue, could you please try to re-run the function to see whether it works now?"
        )
        return(FALSE)
    }
}

test_url_status <- function(url, organism) {

  test_status <- curl::curl_fetch_memory(url)

  if (test_status$status_code == 200) {
    json.qry.info <-
      jsonlite::fromJSON(url)
    return(json.qry.info)
  } else {
    message(
      "Something went wrong when trying to access the url: ",
      url,
      ". It seems like the organism '",
      organism,
      "' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted."
    )
    return(FALSE)
  }
}

#' @title Helper function to test connection and availability of
#' queried FTP files. This version work when ssl certificates are too strict.
#' @description To make sure that the automatically generated query path to
#' the ftp stored file on NCBI or ENSEMBL actually exists, this helper function
#' makes a test query.
#' @author Hajk-Georg Drost
#' @importFrom  RCurl url.exists
#' @importFrom XML getHTMLLinks
#' @noRd
#' @import curl
exists.ftp.file.new <- function(url, file.path) {

  url_dir_safe <- gsub("//$", "/", paste0(dirname(url), "/"))
  if (!RCurl::url.exists(url_dir_safe))
    return(FALSE)

  con <- RCurl::getURL(url_dir_safe, ftp.use.epsv = FALSE, dirlistonly = TRUE)

  dir_files <- XML::getHTMLLinks(con)

  return(is.element(as.character(basename(file.path)),
                    dir_files))
}
