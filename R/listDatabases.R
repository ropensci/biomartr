#' @title Retrieve a List of Available NCBI Databases for Download
#' @description This function allows you to retrieve a list of database names
#' and versions that can be downloaded from correspondning servers.
#' @param db a character string specifying the name of the database that shall
#' be searched for.
#' @param update a logical value specifying whether or not the local
#' listDatabases.txt file shall be updated by remote access to NCBI.
#' @description
#' Database retrieval is crucial for most biological studies and analyses.
#' There is a vast diversity of databases that can be accessed remotely or that
#' can be downloaded to your local machine. This function provides an interface
#' to databases that can be downloaded from NCBI servers and lists all available
#' databases and their database version to be able to select an appropriate
#' database for download with \code{\link{download.database}}.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#' # retrieve all versions of the NCBI 'nr' database that can be downloaded
#' listNCBIDatabases(db = "nr")
#'
#' # analogous:
#' # listNCBIDatabases(db = "cdd")
#' # listNCBIDatabases(db = "nt")
#' # listNCBIDatabases(db = "gss")
#' # listNCBIDatabases(db = "refseq_protein")
#' }
#' @seealso \code{\link{download.database}}, \code{\link{download.database.all}}
#' @export

listDatabases <- function(db = "nr", update = FALSE) {
    if (is.element("listDatabases", as.character(match.call()[[1]])))
        warning(
      "Please use listNCBIDatabases() instead of listDatabases(),
      in the next version of biomartr this function will be deprecated.",
            call. = FALSE
        )


    if (!file.exists(file.path(tempdir(), "_ncbi_downloads"))) {
        dir.create(file.path(tempdir(), "_ncbi_downloads"))
    }

    db.file.name <-
        file.path(tempdir(), "_ncbi_downloads", "listDatabases.txt")

    if (update) {
        if (file.exists(db.file.name))
            unlink(db.file.name)
    }

    if (file.exists(db.file.name)) {
        listDBs <-
            utils::read.csv(db.file.name,
                            sep = ";",
                            header = FALSE)
    } else {
        # retrieve all available databases from NCBI
        listDBs <-
            strsplit(
                RCurl::getURL(
                    url         = refseq_genbank_blast_ftp_server_url_blast(),
                    ftp.use.epsv = FALSE,
                    dirlistonly  = TRUE
                ),
                "\n"
            )

        utils::write.table(
            x         = listDBs[[1]],
            file      = db.file.name,
            quote     = FALSE,
            col.names = FALSE,
            row.names = FALSE,
            sep       = ";"
        )
    }

    if (db == "all") {
        listDBs2 <- listDBs[[1]]
        all_databases <-
            listDBs2[-which(unlist(lapply(listDBs2, function(x)
                stringr::str_detect(x, ".md5"))))]
        # available DBs
        dbs <-
            as.vector(names(table(unlist(
                lapply(all_databases, function(x)
                    unlist(stringr::str_split(x, "[.]"))[1])
            ))))
        return(dbs[-which(dbs == "README")])

    } else {
        listDBs2 <- listDBs[[1]]
        # select all database versions of 'db'
        DBName <-
            listDBs2[unlist(lapply(listDBs2, function(x)
                stringr::str_detect(x, paste0("^", db))))]

        if (length(DBName) == 0)
            stop("No entries for db = '", db, "' could not be found.",
                 call. = FALSE)

        # delete md5 entries
        return(as.vector(DBName[-which(unlist(lapply(DBName, function(x)
            stringr::str_detect(x, ".md5"))))]))
    }
}

#' @export
#' @rdname listDatabases
listNCBIDatabases <- listDatabases

