exists.ftp.file <- function(url, file.path) {
    con <- curl::curl(url = url, "rb", curl::new_handle(dirlistonly = TRUE))
    ftp.content <- suppressMessages(readr::read_delim(con, delim = "\n", col_names = FALSE))
    close(con)
    if (is.element(as.character(basename(file.path)), as.character(ftp.content$X1)))
        return(TRUE)
    else 
        return(FALSE)
}
