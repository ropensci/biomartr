#' @title Helper function to perform customized downloads
#' @description To achieve the most stable download experience,
#' ftp file downloads are customized for each operating system.
#' @param url the url to download
#' @param ... additional arguments that shall be passed to
#' \code{\link[downloader]{download}}
#' @author Hajk-Georg Drost
#' @noRd
custom_download <- function(url, ...) {

  if (url == "not_found" || url == "Not available") {
    message("The FTP url waas 'NA' and thus does not exist. Thus, download has been omitted.")
    return(FALSE)
  }

    # test if internet connection is available
    connected.to.internet()

    # set timeout to 30 minutes
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

#' An wrapper to custom_download which checks if local files exists
#' @noRd
custom_download_check_local <- function(url, local_file, rest_api_status, db = "ensembl",
                                        notify_exist = TRUE, ...) {

  if (!is.logical(local_file) && file.exists(local_file)) {
    if (notify_exist) message("File ", local_file,
                              " exists already. Thus, download has been skipped.")
  } else {
    if (!is.null(rest_api_status) && rest_api_status$release_coord_system_version == "not_found") {
      message("Found organism but given assembly type (toplevel/primary) or release number did not specify existing file
                     in ensembl, did you specify primary for toplevel only? Check that it exists on ensembl through your browser")
      return(FALSE)
    }

    tryCatch({
      custom_download(url, destfile = local_file, mode = "wb")
    }, error = function(e)
      if (db == "ensembl") {
        message(
          "Something went wrong while trying to reach the file '", url,
          "'. This could be due to an instable internet connection or incorrect file path on the ENSEMBL ftp server. Please check if you are able to reach '",url, "' in your web browser.",
          " In some cases ENSEMBL released a new database version and path names or the API weren't updated yet. Please give it a few days time or contact the ENSEMBL helpdesk."
        )
      } else {
        message(
          "The download session seems to have timed out at the FTP site '", url,
          "'. This could be due to an overload of queries to the databases.",
          " Please restart this function to continue the data retrieval process or wait ",
          "for a while before restarting this function in case your IP address was logged due to an query overload on the server side."
        )
      }
      )
  }
  return(invisible(NULL))
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

  url_dir_safe <- unique(gsub("//$", "/", paste0(dirname(url), "/")))
  url_dir_exists <- RCurl::url.exists(url_dir_safe)
  if (!url_dir_exists) return(url_dir_exists)

  con <- RCurl::getURL(url_dir_safe, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  dir_files <- XML::getHTMLLinks(con)
  return(is.element(as.character(basename(file.path)),
                    dir_files))
}
