#' @title Retrieve All Organism Names Stored on FlyMine
#' @description This function extracts all organism names (scientific names) for which genomes,
#' proteomes, and CDS files are stored on the FlyMine server.
#' @param organism a character string specifying the scientific name of an organism for which data information shall be retrieved.
#' Default is \code{organism} = \code{"all"}.
#' @author Hajk-Georg Drost
#' @details
#' 
#' This function queries the FlyMine API and retrieves all organisms specific information
#' stored on FlyMine. This way annotation information can be accessed allowing a better
#' transparency of genome data used for omics studies. 
#' 
#' @references \url{http://www.flymine.org/}
#' @examples
#' 
#' # retrieve data for all available organisms stored in FlyMine 
#' head(flymine.organisms())
#' 
#' # retrieve data for Arabidopsis thaliana available organisms stored in FlyMine
#' flymine.organisms(organism = "Drosophila melanogaster") 
#' 
#' @export

flymine.organisms <- function(organism = "all"){
        
        
        
        if(!file.exists(file.path(tempdir(),"flymineOrgs.tsv"))){
                
                downloader::download( url      = "http://www.flymine.org/release-40.0/service/query/results?query=%3Cquery+name%3D%22%22+model%3D%22genomic%22+view%3D%22Organism.name+Organism.taxonId+Organism.commonName+Organism.genus+Organism.shortName+Organism.species%22+longDescription%3D%22%22+sortOrder%3D%22Organism.name+asc%22%3E%3C%2Fquery%3E&format=tab
",
                                      destfile = file.path(tempdir(),"flymineOrgs.tsv"),
                                      mode     = "wb",
                                      quiet    = TRUE ) 
        }
        
        flymineOrgs <- read.csv(file.path(tempdir(),"flymineOrgs.tsv"), sep = "\t", header = FALSE)
        colnames(flymineOrgs) <- c("Scientific_Name" ,"Taxon_ID","Common_Name","Genus","Short_Name","Species")
        
        if(organism == "all")
                return(flymineOrgs[ , c("Scientific_Name","Common_Name","Genus","Species","Taxon_ID")])
        
        
        if(organism != "all"){
                
                tryCatch({
                        
                        return(flymineOrgs[which(flymineOrgs[ , "Scientific_Name"] == organism) , c("Scientific_Name","Common_Name","Genus","Species","Taxon_ID")])
                        
                }, error = function(e) stop("Your input organism '",organisms,"' could not be found on Phytozome!"))
                
                
        }
        
}








