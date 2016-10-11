#' @title Download a NCBI Database to Your Local Hard Drive
#' @description This function allows you to download a database selected by
#' \code{\link{listDatabases}} to your local hard drive.
#' @param db a character string specifying the database that shall be downloaded (selected from \code{\link{listDatabases}}).
#' @param path a character string specifying the location (a folder) in which the corresponding
#' database shall be stored. Default is \code{path} = \code{"database"}. In case this folder does not exist yet, it will be created.
#' @details
#' This function downloads large databases to your hard drive. For this purpose a folder
#' named \code{database} (default) is created and the correspondning database then stored in this folder. 
#' 
#' @author Hajk-Georg Drost
#' @examples 
#' \dontrun{
#'   
#'   # search for available NCBI nr databases
#'   listDatabases(db = "nr")
#'   
#'   # select NCBI nr version 27 =  "nr.27.tar.gz"
#'   # and download it to your hard drive
#'   # -> please note that large databases take some time for download!
#'   download_database(db = "nr.27.tar.gz")
#' 
#' }
#' @seealso \code{\link{download_database_all}}
#' @export

download_database <- function(db, path = "database"){
        
    if (!file.exists(path))
        dir.create(path)
    
    db.name <- names(table(unlist(sapply(db, function(x)
        unlist(stringr::str_split(x, "[.]"))[1]))))
    
    if (!is.element(db.name, listDatabases("all")))
        stop(
            "The specified database '",
            db,
            "' could not be found on NCBI.
            Please use the listDatabases('all') command to retrieve available databases.",
            call. = FALSE
        )
    
    downloader::download(paste0("ftp://ftp.ncbi.nlm.nih.gov/blast/db/", db),
                         file.path(path, db) ,
                         mode = "wb")
    
    # limit NCBI queries
    Sys.sleep(0.33
    )
}









