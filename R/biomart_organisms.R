

biomart_organisms <- function(organism = NULL){
        
        
        if(!file.exists("_biomart/listMarts.txt")){
                
                if(!file.exists("_biomart"))
                        dir.create("_biomart")
                
                all_marts <- droplevels.data.frame(biomaRt::listMarts())
                write.table(all_marts,"_biomart/listMarts.txt",sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
                
        }
        

        if(file.exists("_biomart/listMarts.txt"))
                all_marts <- read.csv("_biomart/listMarts.txt", header = TRUE, sep = "\t", colClasses = rep("character",2),stringsAsFactors = FALSE)
        
        
        if(!file.exists("_biomart/listDatasets.txt")){
                all_datasets <- do.call(rbind,lapply(as.vector(all_marts[ , "biomart"]), 
                                       function(mart){ df <- as.data.frame(biomaRt::listDatasets(biomaRt::useMart(biomart = mart)))
                                                       df <- dplyr::mutate(df,mart = rep(mart,length(mart)))
                                                       return(df) } ))
                                                        
                
                write.table(all_datasets,"_biomart/listDatasets.txt",sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
        }
        
        if(file.exists("_biomart/listDatasets.txt"))
                all_datasets <- read.csv("_biomart/listDatasets.txt", header = TRUE, sep = "\t", colClasses = rep("character",3),stringsAsFactors = FALSE)
        
  
       all_datasets <- dplyr::mutate(all_datasets,organism_name = sapply(all_datasets[ ,"description"], function(x) paste0(strsplit(x," ")[[1]][1:2],collapse = " ")))
       all_datasets <- dplyr::select(all_datasets, list(organism_name,description,mart,dataset,version))
       
       # find out whether or not the query organisms has an entry 
       if(length(unlist(sapply(all_datasets[ , "organism_name"], function(x) stringr::str_match_all(stringr::fixed(x),organism)))) == 0)
               stop("Unfortunately, no entry for ", organism, " has been found.")
       
       if(!is.null(organism))
               return(dplyr::filter(all_datasets,organism_name == organism))
       
       if(is.null(organism))
               return(all_datasets)
             
}





