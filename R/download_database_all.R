#' @title Download all elements of an NCBI databse
#' @description The \code{\link{download_database}} functions allows users to retrieve individual
#' packages of a NCBI database. This function is designed to retrieve the entire database 
#' selected by the users (hence all packages corresponding to this database).
#' @param db a character string specifying the database that shall be downloaded (selected from \code{\link{listDatabases}}).
#' @param path a character string specifying the location (a folder) in which the corresponding
#' database shall be stored. Default is \code{path} = \code{"DB"}. In case this folder does not exist yet, it will be created.
#' @author Hajk-Georg Drost
#' @examples 
#' \dontrun {
#' # search for available NCBI databases
#'   listDatabases(db_name = "all")
#'   
#' # choose database NCBI nr and download compelete database
#'   download_database_all(name = "nr", path = "database/nr")
#' }
#' @seealso \code{\link{download_database}}
download_database_all <- function(db, path) {
    
    sapply(listDatabases(db), download_database, path = path)
    
}
