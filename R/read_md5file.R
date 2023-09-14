#' @title Helper function to read md5checksum file from NCBI
#' @description NCBI stores md5checksum.txt files that can be retrieved
#' to check whether or not the downloaded file is corrupt.
#' @param md5path path to md5checksum.txt file.
#' @author Hajk-Georg Drost
#' @noRd
#' @export
read_md5file <- function(md5path) {
    
    if (!file.exists(md5path))
        stop("The file path you specified does not seem to exist: '", md5path,"'.", call. = FALSE)
    
    file <-
        readr::read_delim(
            md5path,
            delim = "  ",
            col_names = c("md5", "file_name"),
            col_types = readr::cols(
                md5 = readr::col_character(),
                file_name = readr::col_character()
            )
        )
    
    return(file)
}
