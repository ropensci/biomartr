#' @title Retrieve a List of Available Databases for Download  
#' @description This function allows you to retrieve a list of database names and versions
#' that can be downloaded from correspondning servers.
#' @param db_name a character string specifying the name of the database that shall be searched for.
#' @param db_format a character string specifying the database format, e.g. \code{db_format} = \code{"fasta"}.
#' @param update a logical value specifying whether or not the local listDatabases.txt file shall be updated by remote access to NCBI.
#' @description
#' 
#' Database retrieval is crucial for most biological studies and analyses.
#' There is a vast diversity of databases that can be accessed remotely or that can be downloaded
#' to your local machine. This function provides an interface to databases that can be downloaded 
#' from NCBI servers and lists all available databases and their database version to be able to
#' select an appropriate database for download with \code{\link{download_database}}.
#' 
#' @author Hajk-Georg Drost
#' @references 
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA}
#' @examples
#' 
#' # retrieve all versions of the NCBI 'nr' database that can be downloaded
#' # listDatabases(db_name = "nr", db_format = "fasta")
#' 
#' # analogous:
#' # listDatabases(db_name = "cdd", db_format = "fasta")
#' # listDatabases(db_name = "nt", db_format = "fasta")
#' # listDatabases(db_name = "gss", db_format = "fasta")
#' # listDatabases(db_name = "refseq_protein", db_format = "fasta")
#' 
#' @seealso \code{\link{download_database}}
#' @export

listDatabases <- function(db_name = "nr", db_format = "fasta", update = FALSE){
        
        
        if(db_format == "fasta"){
                
                if(!file.exists(file.path(tempdir(),"_ncbi_downloads"))){
                        
                        dir.create(file.path(tempdir(),"_ncbi_downloads"))
                        
                }
                
                if(update){
                        
                        if(file.exists(file.path(tempdir(),"_ncbi_downloads","listDatabases.txt")))
                                unlink(file.path(tempdir(),"_ncbi_downloads","listDatabases.txt"))
                        
                }
                
                
                if(file.exists(file.path(tempdir(),"_ncbi_downloads","listDatabases.txt"))){
                        
                        listDBs <- utils::read.csv(file.path(tempdir(),"_ncbi_downloads","listDatabases.txt"), sep = ";", header = FALSE)
                        
                } else {
                        
                        # retrieve all avaiable databases from NCBI 
                        listDBs <- strsplit( RCurl::getURL( url         = "ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA",
                                                            ftp.use.epsv = FALSE, 
                                                            dirlistonly  = TRUE ), "\n" )
                        
                        utils::write.table( x         = listDBs[[1]],
                                            file      = file.path(tempdir(),"_ncbi_downloads","listDatabases.txt"), 
                                            quote     = FALSE, 
                                            col.names = FALSE, 
                                            row.names = FALSE,
                                            sep       = ";" )
                        
                        listDBs <- listDBs[[1]]
                }
                
                if (db_name == "all"){
                        return(listDBs[-which(sapply(listDBs,function(x) stringr::str_detect(x,".md5")))])
                } else {
                        
                        # select all database versions of 'db_name'
                        DBName <- listDBs[sapply(listDBs,function(x) stringr::str_detect(x,paste0("^",db_name)))]
                        
                        if(length(DBName) == 0)
                                stop("No entries for db_name = '",db_name,"' could not be found.")
                        
                        # limit NCBI queries
                        if(!file.exists(file.path(tempdir(),"_ncbi_downloads","listDatabases.txt")))
                                Sys.sleep(0.33)
                        
                        # delete md5 entries
                        return(DBName[-which(sapply(DBName,function(x) stringr::str_detect(x,".md5")))])
                        
                }
        }
}

