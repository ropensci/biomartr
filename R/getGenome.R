#' @title Genome Retrieval
#' @description This function retrieves a fasta-file storing the genome of an organism of interest and stores
#' the genome file in the folder '_ncbi_downloads/genomes'.
#' @param db a character string specifying the database from which the genome shall be retrieved: 'refseq'.
#' Right now only the ref seq database is included. Later version of \pkg{biomartr} will also allow
#' sequence retrieval from additional databases.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other". 
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis thaliana'.
#' @param store_at a character string specifying the location (a folder) in which the corresponding
#' genome shall be stored. Default is \code{store_at} = \code{file.path("_ncbi_downloads","genomes")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/}
#' 
#' 
#' and creates a directory '_ncbi_downloads/genomes' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomes' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return A data.table storing the geneids in the first column and the DNA dequence in the second column.
#' @examples \dontrun{
#' 
#' # download the genome of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' getGenome( db       = "refseq", 
#'            kingdom  = "plant", 
#'            organism = "Arabidopsis thaliana", 
#'            store_at = file.path("_ncbi_downloads","genomes"))
#' 
#' file_path <- file.path("_ncbi_downloads","genomes","Arabidopsis_thaliana_genome.fna.gz")
#' Ath_genome <- read_genome(file_path, format = "fasta")
#' 
#' }
#' @references 
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq}
#' 
#' \url{http://www.ncbi.nlm.nih.gov/refseq/about/}
#' 
#' @seealso \code{\link{read_genome}}
#' @export
getGenome <- function(db = "refseq", kingdom, organism, store_at = file.path("_ncbi_downloads","genomes")){
        
        if(!is.element(db,c("refseq")))
                stop("Please select one of the available data bases: 'refseq'")
        
        if(!is.genome.available(organism = organism))
                stop(paste0("Unfortunately for '",organism,"' no genome is stored on NCBI."))
        
        
        if(db == "refseq"){
                
                
                if(!file.exists(store_at)){
                        
                        dir.create(store_at)
                        
                }
                
                subfolders <- c("archaea","bacteria", "fungi", "invertebrate", "plant",
                                "protozoa", "vertebrate_mammalian", "vertebrate_other")
                
                if(!is.element(kingdom,subfolders))
                        stop(paste0("Please select a valid kingdom: ",paste0(subfolders,collapse = ", ")))
                
                url_organisms <- try(RCurl::getURL(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/"),
                                                   ftp.use.epsv = FALSE, dirlistonly = TRUE))
                
                # replace white space in scientific name with "_"
                # to match the corresponding folder on the NCBI server
                # e.g. "Arabodopsis thaliana" will become "Arabodopsis_thaliana"
                organism <- stringr::str_replace(organism," ","_")
                
                check_organisms <- strsplit(url_organisms,"\n")
                
                if(!is.element(organism,unlist(check_organisms)))
                        stop("Please choose a valid organism.")
                
                url_lates_version <- try(RCurl::getURL(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                                                              organism,"/latest_assembly_versions/"), ftp.use.epsv = FALSE, dirlistonly = TRUE))
                
                url_lates_version <- unlist(strsplit(url_lates_version,"\n"))
                
                query_url_list_files <- paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                                               organism,"/latest_assembly_versions/",url_lates_version)
                
                query_url_list_files_raw <- try(RCurl::getURL(query_url_list_files, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE))
                
                query_url_list_files <- unlist(strsplit(query_url_list_files_raw,"\n"))
                
#                 organism_file_match <- stringr::str_match(query_url_list_files, pattern = "*_genomic.fna.gz")
#                 organism_file <- query_url_list_files[!is.na(organism_file_match)]
                
                file_path <- file.path(store_at,paste0(organism,"_genome.fna.gz"))
                
                if(!file.exists(file_path)){
                        

                        downloader::download(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                                             organism,"/latest_assembly_versions/",url_lates_version,"/",
                                             paste0(query_url_list_files,"_genomic.fna.gz")), file.path(store_at,paste0(organism,"_genome.fna.gz")),
                                             mode = "wb")
                        
                        # NCBI limits requests to three per second
                        Sys.sleep(0.33)

                }
                
        }

      print(paste0("The genome of '",organism,"' has been downloaded to '",store_at,"' and has been named '",paste0(organism,"_genome.fna.gz"),"' ."))  
}







