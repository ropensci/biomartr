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
#' # search for available NCBI databases
#'   listNCBIDatabases(db = "all")
#' \dontrun{  
#' # choose database NCBI nr and download compelete database
#'   download.database.all(name = "nr", path = "nr")
#' }
#' @seealso \code{\link{download.database}}, \code{\link{listNCBIDatabases}}
#' @return A character vector storing the file paths of the downloaded databases.
#' @export

download.database.all <- function(db, path = "database") {
    message("Starting download of the files: ",
            paste0(listDatabases(db = db), collapse = ", "),
            " ...")
    dld_paths <- unlist(lapply(listNCBIDatabases(db = db), download.database, path = path))
    message("Download process is finished and files are stored in '",
            path, "'.")
    return(dld_paths)
}
