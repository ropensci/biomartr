#' @title Function to list all genomes available on the NCBI ftp:// server.
#' @description This function retrieves the names of all genomes available on the NCBI ftp:// server and stores
#' the results in a file named 'overview.txt' inside the directory '_ncbi_downloads' that
#' is built inside the workspace. 
#' @param kingdom a character string specifying a potential filter of available genomes: "all","Archaea", "Bacteria", "Eukaryota", "Viroids", "Viruses".
#' @param details a boolean value specifying whether only the scientific names of stored genomes shall be returned
#' (details = FALSE) or all information such as "organism_name","kingdoms", "group","subgroup","file_size_MB",
#' "chrs","organelles","plasmids", and "bio_projects".
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/ and creates a directory '_ncbi_downloads' to store
#' the overview.txt file for future processing. In case the overview.txt file already exists within the
#' '_ncbi_downloads' folder and is accessible within the workspace, no download process will be performed.
#' @return A data.frame storing either the organism names (details = FALSE)
#' or all information present on the NCBI database (details = TRUE).
#' @examples \dontrun{
#' 
#' # the simplest way to retrieve all names of genomes stored within NCBI databases
#' head(listGenomes() , 5)
#' 
#' # show all details
#' head(listGenomes(details = TRUE) , 5)
#' 
#' # show all details only for Bacteria
#' head(listGenomes(kingdom = "Bacteria", details = TRUE) , 5)
#' 
#' # in case you are interested in the number of genomes available for each kingdom, run:
#' 
#' ncbi_genomes <- listGenomes(details = TRUE)
#' table(ncbi_genomes[ , "kingdoms"])
#' 
#' # analogous, if you are interested in the number of genomes available for each group, run:
#' ncbi_genomes <- listGenomes(details = TRUE)
#' table(ncbi_genomes[ , "group"])
#' 
#' # for subgroup
#' table(ncbi_genomes[ , "subgroup"])
#' 
#' # order by file size
#' library(dplyr)
#' head(arrange(ncbi_genomes, desc(file_size_MB)) , 5)
#' 
#' }
#' @references ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt
#' @export
listGenomes <- function(kingdom = "all", details = FALSE){
        
        
        if(!is.element(kingdom,c("all","Archaea", "Bacteria", "Eukaryota", "Viroids", "Viruses")))
                stop("Please use a valid kingdom.")
        
        if(!file.exists("_ncbi_downloads")){
                
                dir.create("_ncbi_downloads")
                
        }
        
        if(!file.exists("_ncbi_downloads/overview.txt")){
                
                download.file("ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt","_ncbi_downloads/overview.txt", quiet = TRUE)
                
        }
        
        col_classes <- vector(mode = "character",length = 9)
        col_classes <- c(rep("character",4),rep("numeric",5))
        ncbi_overview <- read.csv("_ncbi_downloads/overview.txt",
                                  sep = "\t", header = TRUE,
                                  colClasses = col_classes,
                                  na.strings = "-")
        
        names(ncbi_overview) <- c("organism_name","kingdoms",
                                  "group","subgroup","file_size_MB",
                                  "chrs","organelles","plasmids",
                                  "bio_projects")
        
        if(kingdom == "all"){
                if(details == TRUE){
                        return(ncbi_overview)
                }
                
                if(details == FALSE){
                        return(ncbi_overview[ , "organism_name"])
                }
        }
        
        if(kingdom != "all"){
                if(details == TRUE){
                        return( dplyr::filter(ncbi_overview, kingdoms == kingdom) )
                }
                
                if(details == FALSE){
                        return( dplyr::select(dplyr::filter(ncbi_overview,kingdoms == kingdom),
                                              organism_name) )
                }
        }
        
}

