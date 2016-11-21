#' @title Retrieve a List of Available NCBI Databases for Download  
#' @description This function allows you to retrieve a list of database names and versions
#' that can be downloaded from correspondning servers.
#' @param db a character string specifying the name of the database that shall be searched for.
#' @param update a logical value specifying whether or not the local listDatabases.txt file shall be updated by remote access to NCBI.
#' @description
#' 
#' Database retrieval is crucial for most biological studies and analyses.
#' There is a vast diversity of databases that can be accessed remotely or that can be downloaded
#' to your local machine. This function provides an interface to databases that can be downloaded 
#' from NCBI servers and lists all available databases and their database version to be able to
#' select an appropriate database for download with \code{\link{download.database}}.
#' 
#' @author Hajk-Georg Drost
#' @examples
#' 
#' # retrieve all versions of the NCBI 'nr' database that can be downloaded
#' # listDatabases(db = "nr")
#' 
#' # analogous:
#' # listDatabases(db = "cdd")
#' # listDatabases(db = "nt")
#' # listDatabases(db = "gss")
#' # listDatabases(db = "refseq_protein")
#' 
#' @seealso \code{\link{download.database}}
#' @export

listDatabases <- function(db = "nr", update = FALSE) {
    
    if (!file.exists(file.path(tempdir(), "_ncbi_downloads"))) {
        dir.create(file.path(tempdir(), "_ncbi_downloads"))
        
    }
    
    if (update) {
        if (file.exists(file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt")))
            unlink(file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt"))
        
    }
    
    
    if (file.exists(file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt"))) {
        listDBs <-
            utils::read.csv(
                file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt"),
                sep = ";",
                header = FALSE
            )
        
    } else {
        # retrieve all avaiable databases from NCBI
        listDBs <-
            strsplit(
                RCurl::getURL(
                    url         = "ftp://ftp.ncbi.nlm.nih.gov/blast/db/",
                    ftp.use.epsv = FALSE,
                    dirlistonly  = TRUE
                ),
                "\n"
            )
        
        utils::write.table(
            x         = listDBs[[1]],
            file      = file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt"),
            quote     = FALSE,
            col.names = FALSE,
            row.names = FALSE,
            sep       = ";"
        )
    }
    
    if (db == "all") {
        listDBs2 <- listDBs[[1]]
        all_databases <-
            listDBs2[-which(sapply(listDBs2, function(x)
                stringr::str_detect(x, ".md5")))]
        # available DBs
        dbs <-
            as.vector(names(table(unlist(
                sapply(all_databases, function(x)
                    unlist(stringr::str_split(x, "[.]"))[1])
            ))))
        return(dbs[-which(dbs == "README")])
        
    } else {
        listDBs2 <- listDBs[[1]]
        # select all database versions of 'db'
        DBName <-
            listDBs2[sapply(listDBs2, function(x)
                stringr::str_detect(x, paste0("^", db)))]
        
        if (length(DBName) == 0)
            stop("No entries for db = '", db, "' could not be found.", call. = FALSE)
        
        # limit NCBI queries
        if (!file.exists(file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt")))
            Sys.sleep(0.33)
        
        # delete md5 entries
        return(as.vector(DBName[-which(sapply(DBName, function(x)
            stringr::str_detect(x, ".md5")))]))
        
    }
}

