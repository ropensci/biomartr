#' @title Retrieve All Organism Names Stored on Phytozome v10
#' @description This function extracts all organism names (scientific names) for which genomes,
#' proteomes, and CDS files are stored on the Phytozome v10 server.
#' @param organism a character string specifying the scientific name of an organism for which data information shall be retrieved.
#' Default is \code{organism} = \code{"all"}.
#' @author Hajk-Georg Drost
#' @details
#' 
#' This function queries the Phytozome v10 API and retrieves all organisms specific information
#' stored on Phytozome. This way annotation information can be accessed allowing a better
#' transparency of genome data used for omics studies. 
#' 
#' @references \url{http://phytozome.jgi.doe.gov/phytomine/begin.do}
#' @examples
#' 
#' # retrieve data for all available organisms stored in Phytozome 
#' head(phytozomeOrganisms())
#' 
#' # retrieve data for Arabidopsis thaliana available organisms stored in Phytozome
#' phytozomeOrganisms(organism = "Arabidopsis thaliana") 
#' 
#' @export

phytozomeOrganisms <- function(organism = "all"){
        
        
        
        if(!file.exists(file.path(tempdir(),"phytozomeOrgs.tsv"))){
                
                downloader::download( url      = "http://phytozome.jgi.doe.gov/phytomine/service/query/results?query=%3Cquery+name%3D%22%22+model%3D%22genomic%22+view%3D%22Organism.annotationVersion+Organism.name+Organism.commonName+Organism.taxonId+Organism.version+Organism.assemblyVersion+Organism.genus+Organism.proteomeId+Organism.species%22+longDescription%3D%22%22+sortOrder%3D%22Organism.annotationVersion+asc%22%3E%3C%2Fquery%3E&format=tab",
                                      destfile = file.path(tempdir(),"phytozomeOrgs.tsv"),
                                      mode     = "wb",
                                      quiet    = TRUE ) 
        }
        
        phytozomeOrgs <- read.csv(file.path(tempdir(),"phytozomeOrgs.tsv"), sep = "\t", header = FALSE)
        colnames(phytozomeOrgs) <- c("Annotation_Version", "Scientific_Name" ,"Common_Name","Taxon_ID","Version","Assembly_Version","Genus","Proteome_ID","Species")
        
        if(organism == "all")
                return(phytozomeOrgs[ , c("Scientific_Name","Common_Name","Annotation_Version","Assembly_Version","Proteome_ID","Version","Genus","Species","Taxon_ID")])
        
        
        if(organism != "all"){
                
                tryCatch({
                        
                        return(phytozomeOrgs[which(phytozomeOrgs[ , "Scientific_Name"] == organism) , c("Scientific_Name","Common_Name","Annotation_Version","Assembly_Version","Proteome_ID","Version","Genus","Species","Taxon_ID")])
                        
                }, error = function(e) stop("Your input organism '",organisms,"' could not be found on Phytozome!"))
                
                
        }
        
}







