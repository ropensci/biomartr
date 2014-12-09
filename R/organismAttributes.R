

organismAttributes <- function(organism, update = FALSE, topic = NULL){
        
        orgBM <- organismBM(organism = organism, update = update)
        orgMarts <- names(table(orgBM[ , "mart"]))
        martList <- lapply(orgMarts, function(mart) dplyr::filter(orgBM,mart == mart))
        
        fsep <- .Platform$file.sep
        attrTXT <- paste0("listAttributes_",stringr::str_replace(organism," ","_"))
        
        if(!file.exists("_biomart")){
                
                dir.create("_biomart")
        }       
        
        if(!file.exists(paste0("_biomart",fsep,attrTXT,".txt"))){
                        
                        attrList <- lapply(martList, function(mart) { 
                                
                                mart <- as.data.frame(mart); 
                                mart_tbl <- do.call(rbind,lapply(1:nrow(mart),
                                                                 function(dataset) {
                                                                         
                                                                         attr_tbl <- biomaRt::listAttributes(
                                                                                 biomaRt::useDataset(dataset = mart[ dataset , "dataset"], 
                                                                                                     mart    = biomaRt::useMart(mart[ dataset , "mart"])))
                                                                         
                                                                         datasetVec <- rep(mart[ dataset , "dataset"], nrow(attr_tbl))
                                                                         
                                                                         attr_tbl <- dplyr::mutate(attr_tbl, dataset = datasetVec)
                                                                         
                                                                         return(attr_tbl)
                                                                 }))
                                
                                martVec <-rep(mart[1  , "mart"], nrow(mart_tbl))
                                mart_tbl <- dplyr::mutate(mart_tbl, mart = martVec)
                                return(mart_tbl)
                        }
                        )
                        
                        write.table(do.call(rbind,attrList), paste0("_biomart",fsep,attrTXT,".txt"), sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
                
        }
        
        attributeTable <- read.csv(paste0("_biomart",fsep,attrTXT,".txt"), sep = "\t",header = TRUE, colClasses = rep("character",4), stringsAsFactors = FALSE)        
        
        summ_attrTable <- dplyr::summarise(dplyr::group_by(attributeTable, name), mart = names(table((mart))), dataset = names(table((dataset))))
        
        if(!is.null(topic)){
                
                findTopic <- which(sapply(summ_attrTable[ , "name"],function(x) stringr::str_detect(x,topic)))
                
                if(dim(summ_attrTable[findTopic , ])[1] == 0)
                        stop("Unfortunately the topic '", topic ,"' could not be found.")
                
                return(summ_attrTable[findTopic , ])
                
        } else {
                
                return(summ_attrTable)
                
        }
        
}






