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

#' @title Function for downloading a specific genome stored on the NCBI ftp:// server.
#' @description This function retrieves a fasta-file storing the genome of an organism of interest and stores
#' the genome file in the folder '_ncbi_downloads/genomes'.
#' @param db a character string specifying the database from which the genome shall be retrieved: 'refseq','genebank', or 'all'.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other" 
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis_thaliana'.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genebank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genebank/
#' 
#'  all: ftp://ftp.ncbi.nlm.nih.gov/genomes/all/
#' 
#' and creates a directory '_ncbi_downloads/genomes' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomes' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return A data.table storing the geneids in the first column and the DNA dequence in the second column.
#' @examples \dontrun{
#' # download the genome of Arabidopsis thaliana
#' 
#' }
#' @references ftp://ftp.ncbi.nlm.nih.gov/genomes/
#' @export
getGenome <- function(db = "refseq", kingdom = "plant", organism){
        
        if(!is.element(db,c("refseq","genebank","all")))
                stop("Please select one of the available data bases: 'refseq','genebank' or 'all'")

        if(db == "refseq"){
                
                
                if(!file.exists("_ncbi_downloads/genomes")){
                        
                        dir.create("_ncbi_downloads/genomes")
                        
                }
                
                subfolders <- c("archaea","bacteria", "fungi", "invertebrate", "plant",
                                "protozoa", "vertebrate_mammalian", "vertebrate_other")
                
                if(!is.element(kingdom,subfolders))
                        stop(paste0("Please select a valid kingdom: ",subfolders))
                
                url_organisms <- try(RCurl::getURL(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/"),
                                                  ftp.use.epsv = FALSE, dirlistonly = TRUE))
                
                check_organisms <- strsplit(url_organisms,"\n")
                
                if(!is.element(organism,unlist(check_organisms)))
                        stop("Please choose a valid organism.")
                           
                url_lates_version <- try(RCurl::getURL(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                              organism,"/latest_assembly_versions/"), ftp.use.epsv = FALSE, dirlistonly = TRUE))
                
                url_lates_version <- unlist(strsplit(url_lates_version,"\n"))
                
                query_url_list_files <- paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                                    organism,"latest_assembly_versions/",url_lates_version)
                
                query_url_list_files_raw <- try(RCurl::getURL(query_url_list_files, ftp.use.epsv = FALSE, dirlistonly = TRUE))
                query_url_list_files <- unlist(strsplit(query_url_list_files_raw,"\n"))
                
                organism_file_match <- stringr::str_match(query_url_list_files, pattern = "*_genomic.fna.gz")
                organism_file <- query_url_list_files[!is.na(organism_file_match)]
                
                file_path <- paste0("_ncbi_downloads/genomes/",organism,"_genome.fna")
                
                if(!file.exists(file_path)){
                        
                        # http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
                        temp <- tempfile()
                        download.file(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                                             organism,"/latest_assembly_versions/",url_lates_version,"/",
                                             organism_file), paste0("_ncbi_downloads/genomes/",organism,"_genome.fna.gz"))
                        unz(temp, file_path)
                        unlink(temp)
                }
                
                genome <- read.genome(file_path, format = "fasta")
              
        }
        
        return(genome)
}


