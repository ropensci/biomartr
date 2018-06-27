#' @title Download all elements of an NCBI databse
#' @description The \code{\link{download.database}} functions allows users to
#' retrieve individual packages of a NCBI database. This function is designed to
#' retrieve the entire database selected by the users (hence all packages 
#' corresponding to this database).
#' @param db a character string specifying the database that shall be downloaded
#' (selected from \code{\link{listDatabases}}).
#' @param path a character string specifying the location (a folder) in which
#' the corresponding
#' database shall be stored. In case this folder does not exist yet,
#' it will be created.
#' @author Hajk-Georg Drost
#' @examples 
#' \dontrun{
#' # search for available NCBI databases
#'   listNCBIDatabases(db = "all")
#' # choose database NCBI nr and download compelete database
#'   download.database.all(db = "nr", path = "nr")
#' }
#' @seealso \code{\link{download.database}}, \code{\link{listNCBIDatabases}}
#' @return A character vector storing the file paths of the downloaded databases.
#' @export

download.database.all <- function(db, path = NULL) {
    
    db_chunks <- listNCBIDatabases(db = db)
    
    message("Starting download of the files: ",
            paste0(db_chunks, collapse = ", "),
            " ...")
    message("This download process may take a while due to the large size of the individual data chunks ...")
    
    if (is.null(path)) {
        path <- db
    }
    
    dld_paths <- list(length(db_chunks))
    
    for (i in seq_len(length(db_chunks))) {
        dld_paths[i] <- list(download.database(db = db_chunks[i], path = path))
    }
    
    corrupt_md5 <- any(unlist(lapply(dld_paths, is.logical)))
    which_corrupter_md5 <- which(unlist(lapply(dld_paths, is.logical)))
    
    if (corrupt_md5)
        warning("The file(s) ", paste0(db_chunks[which_corrupter_md5], collapse = ", "), " had corrupted md5 check sum(s). You can simply re-run this function to re-download corrupted files.")
    
    message("Download process is finished and files are stored in '",
            path, "'.")
    
    if (corrupt_md5)
        return(unlist(dld_paths[-which_corrupter_md5]))
    
    if (!corrupt_md5)
        return(unlist(dld_paths))
}
