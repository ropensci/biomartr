#' @title Retrieve All Organism Names Stored on ZebrafishMine
#' @description This function extracts all organism names (scientific names) for which genomes,
#' proteomes, and CDS files are stored on the ZebrafishMine server.
#' @param organism a character string specifying the scientific name of an organism for which data information shall be retrieved.
#' Default is \code{organism} = \code{"all"}.
#' @author Hajk-Georg Drost
#' @details
#' 
#' This function queries the ZebrafishMine API and retrieves all organisms specific information
#' stored on ZebrafishMine. This way annotation information can be accessed allowing a better
#' transparency of genome data used for omics studies. 
#' 
#' @references \url{http://zebrafishmine.org/begin.do}
#' @examples
#' 
#' # retrieve data for all available organisms stored in ZebrafishMine 
#' head(zebrafishmine.organisms())
#' 
#' # retrieve data for Arabidopsis thaliana available organisms stored in ZebrafishMine
#' zebrafishmine.organisms(organism = "Danio rerio") 
#' 
#' @export

zebrafishmine.organisms <- function(organism = "all"){
        
        organisms <- NULL
        
        if(!file.exists(file.path(tempdir(),"zebrafishmineOrgs.tsv"))){
                
                downloader::download( url      = "http://zebrafishmine.org/service/query/results?query=%3Cquery+name%3D%22%22+model%3D%22genomic%22+view%3D%22Organism.commonName+Organism.genus+Organism.name+Organism.shortName+Organism.species+Organism.taxonId%22+longDescription%3D%22%22+sortOrder%3D%22Organism.commonName+asc%22%3E%3C%2Fquery%3E&format=tab",
                                      destfile = file.path(tempdir(),"zebrafishmineOrgs.tsv"),
                                      mode     = "wb",
                                      quiet    = TRUE ) 
        }
        
        zebrafishmineOrgs <- read.csv(file.path(tempdir(),"zebrafishmineOrgs.tsv"), sep = "\t", header = FALSE)
        colnames(zebrafishmineOrgs) <- c("Common_Name","Genus","Scientific_Name","Short_Name","Species","Taxon_ID")
        
        if(organism == "all")
                return(zebrafishmineOrgs[ , c("Scientific_Name","Common_Name","Genus","Species","Taxon_ID")])
        
        
        if(organism != "all"){
                
                tryCatch({
                        
                        return(zebrafishmineOrgs[which(zebrafishmineOrgs[ , "Scientific_Name"] == organism) , c("Scientific_Name","Common_Name","Genus","Species","Taxon_ID")])
                        
                }, error = function(e) stop("Your input organism '",organisms,"' could not be found on Phytozome!"))
                
                
        }
        
}









