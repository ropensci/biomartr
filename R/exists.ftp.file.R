#' @import curl
exists.ftp.file <- function(url, file.path) {
        tryCatch({
                con <-
                        curl::curl(url = url,
                                   open = "rb",
                                   curl::new_handle(dirlistonly = TRUE))
        }, error = function(e) stop("Something went wrong with the connection to: ",url)) 
    ftp.content <- suppressMessages(readr::read_delim(con, delim = "\n", col_names = FALSE))
    close(con)
    if (is.element(as.character(basename(file.path)), as.character(ftp.content$X1)))
        return(TRUE)
    else 
        return(FALSE)
}
