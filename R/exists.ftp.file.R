#' @title Helper function to test connection and availability of
#' queried FTP files.
#' @description To make sure that the automatically generated query path to
#' the ftp stored file on NCBI or ENSEMBL actually exists, this helper function
#' makes a test query.
#' @author Hajk-Georg Drost
#' @noRd
#' @import curl
exists.ftp.file <- function(url, file.path) {

    tryCatch({test_connection <- curl::curl_fetch_memory(url = url)}, error = return(FALSE))

    if (test_connection$status_code == 226) {
        con <-
            curl::curl(url = url,
                       open = "rb",
                       curl::new_handle(dirlistonly = TRUE))

        ftp.content <-
            suppressMessages(readr::read_delim(con,
                                               delim = "\n",
                                               col_names = FALSE))
        close(con)
        return(is.element(as.character(basename(file.path)),
                          as.character(ftp.content$X1)))
    } else {
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
#' @noRd
#' @import curl
exists.ftp.file.new <- function(url, file.path) {


    if (!RCurl::url.exists(dirname(url)))
        return(FALSE)

    con <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

    ftp.content <-
        suppressMessages(readr::read_delim(con,
                                           delim = "\n",
                                           col_names = FALSE))

    return(is.element(as.character(basename(file.path)),
                      as.character(ftp.content$X1)))
}
