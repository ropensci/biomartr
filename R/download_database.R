#' @title Download a NCBI Database to Your Local Hard Drive
#' @description This function allows users to download a database selected by
#' \code{\link{listDatabases}} to their local hard drive.
#' @param db a character string specifying the database that shall be downloaded
#'  (selected from \code{\link{listDatabases}}).
#' @param path a character string specifying the location (a folder) in
#' which the corresponding database shall be stored.
#' Default is \code{path = "database"}.
#' In case this folder does not exist yet, it will be created.
#' @details
#' This function downloads large databases to your hard drive.
#' For this purpose a folder
#' named \code{database} (default) is created and the correspondning
#' database then stored in this folder.
#' @author Hajk-Georg Drost
#' @examples
#'   # search for available NCBI nr databases
#'   listNCBIDatabases(db = "nr")
#' \dontrun{
#'   # select NCBI nr version 27 =  "nr.27.tar.gz"
#'   # and download it to your hard drive
#'   # -> please note that large databases take some time for download!
#'   download.database(db = "nr.27.tar.gz")
#' }
#' @seealso \code{\link{download.database.all}}, \code{\link{listDatabases}}
#' @return File path to the downloaded database file.
#' @export

download.database <- function(db, path = "database") {
    # test if internet connection is available
    connected.to.internet()

    if (!is.element(db, listNCBIDatabases(db = db)))
        stop(
            paste0("The specified database '",
            db,
            "' could not be found on NCBI.",
            "Please use the listNCBIDatabases() command to retrieve available ", 
            "databases or check if the name was written correctly.", 
            collapse = ""),
            call. = FALSE
        )
    
    message("Starting download process of file: ", db, " ...")
    
    if (!file.exists(path))
        dir.create(path)
    
    tryCatch({
        custom_download(
            paste0("ftp://ftp.ncbi.nlm.nih.gov/blast/db/", db),
            file.path(path, db),
            mode = "wb"
        )
        
        custom_download(
            paste0(
                "ftp://ftp.ncbi.nlm.nih.gov/blast/db/",
                paste0(db, ".md5")
            ),
            file.path(path, paste0(db, ".md5")),
            mode = "wb"
        )
    }, error = function(e)
        stop(
            paste0("The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/blast/db/",
            db,
            "' cannot be reached. Are you connected to the internet or did ",
            "something go wrong with the connection to the NCBI server?", 
            collapse = ""),
            call. = FALSE
        ))
    
    # test check sum
    md5_file <-
        readr::read_lines(file.path(path, paste0(db, ".md5")))
    md5_sum <- unlist(stringr::str_split(md5_file, " "))[1]
    
    message("Checking md5 hash of file: ", db , " ...")
    if (!(tools::md5sum(file.path(path, db)) == md5_sum))
        stop(
            paste0("Please download the file '",
            db,
            "' again. The md5 hash between the downloaded file and the file ", 
            "stored at NCBI do not match.", collapse = "")
        )
    unlink(file.path(path, paste0(db, ".md5")))
    message("The md5 hash of file '", db, "' matches!")
    message("File '",
            file.path(path, db),
            " has successfully been retrieved.")
    return(file.path(path, db))
}
