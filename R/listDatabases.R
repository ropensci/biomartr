listDatabases <- function(db_name = "nr", db_format = "fasta", update = FALSE){
        
        
        if(db_format == "fasta"){
                
                if(!file.exists("_ncbi_downloads")){
                        
                        dir.create("_ncbi_downloads")
                        
                }
                
                if(update){
                        
                        if(file.exists(file.path("_ncbi_downloads","listDatabases.txt")))
                                unlink(file.path("_ncbi_downloads","listDatabases.txt"))
                        
                }
                
                
                if(file.exists(file.path("_ncbi_downloads","listDatabases.txt"))){
                        
                        listDBs <- read.csv(file.path("_ncbi_downloads","listDatabases.txt"), sep = ";", header = FALSE)
                        
                } else {
                        
                        # retrieve all avaiable databases from NCBI 
                        listDBs <- strsplit( RCurl::getURL( url         = "ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA",
                                                            ftp.use.epsv = FALSE, 
                                                            dirlistonly  = TRUE ), "\n" )
                        
                        write.table( x         = listDBs[[1]],
                                     file      = file.path("_ncbi_downloads","listDatabases.txt"), 
                                     quote     = FALSE, 
                                     col.names = FALSE, 
                                     row.names = FALSE,
                                     sep       = ";" )
                        
                        listDBs <- listDBs[[1]]
                }
                
                # select all database versions of 'db_name'
                DBName <- listDBs[sapply(listDBs,function(x) stringr::str_detect(x,paste0("^",db_name)))]
                
                # limit NCBI queries
                if(!file.exists(file.path("_ncbi_downloads","listDatabases.txt")))
                        Sys.sleep(0.33)
                
                # delete md5 entries
                return(DBName[-which(sapply(DBName,function(x) stringr::str_detect(x,".md5")))])
                
        }
        
}

