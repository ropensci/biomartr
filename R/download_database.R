#' @title Download a Database to Your Local Hard Drive
#' @description This function allows you to download a database selected by
#' \code{\link{listDatabases}} to your local hard drive.
#' @param name a character string specifying the database that shall be downloaded (selected from \code{\link{listDatabases}}).
#' @param db_format a character string specifying database format, e.g. \code{db_format} = \code{"blastdb"} or \code{db_format} = \code{"fasta"}.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' database shall be stored. Default is \code{path} = \code{"DB"}. In case this folder does not exist yet, it will be created.
#' @details
#' This function downloads large databases to your hard drive. For this purpose a folder
#' named \code{DB} (default) is created and the correspondning database then stored in this folder. 
#' 
#' @author Hajk-Georg Drost
#' @examples 
#' \dontrun{
#'   
#'   # search for available NCBI nr databases
#'   listDatabases(db_name = "nr")
#'   
#'   # select NCBI nr version 27 =  "nr.27.tar.gz"
#'   # and download it to your hard drive
#'   # -> please note that large databases take some time for download!
#'   download_database(name = "nr.27.tar.gz", db_format = "blastdb")
#' 
#' }
#' @export

download_database <- function(name, db_format = "blastdb", path = "DB"){
        
        if (!is.element(db_format,c("blastdb","fasta")))
                stop("db_format = '",db_format,"' is not supported by this function.")
        
        if (!file.exists(path))
                dir.create(path)
        
        if (db_format == "blastdb"){
                downloader::download(paste0("ftp://ftp.ncbi.nlm.nih.gov/blast/db/",name), file.path(path,name) , mode = "wb")
        }
        
        if (db_format == "fasta"){
                downloader::download(paste0("ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/",name), file.path(path,name) , mode = "wb")
        }
        
        # limit NCBI queries
        Sys.sleep(0.33)
}









