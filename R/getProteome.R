#' @title Proteome Retrieval
#' @description This function retrieves a fasta-file storing the proteome of an organism of interest and stores
#' the proteome file in the folder '_ncbi_downloads/proteomes'.
#' @param db a character string specifying the database from which the proteome shall be retrieved: 'refseq'.
#' Right now only the ref seq database is included. Later version of \pkg{biomartr} will also allow
#' sequence retrieval from additional databases.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other". 
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis thaliana'.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' proteome shall be stored. Default is \code{path} = \code{file.path("_ncbi_downloads","proteomes")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/}
#' 
#' 
#' and creates a directory '_ncbi_downloads/proteomes' to store
#' the proteome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomes' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return A data.table storing the geneids in the first column and the protein dequence in the second column.
#' @examples \dontrun{
#' 
#' # download the proteome of Arabidopsis thaliana from refseq
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' getProteome( db       = "refseq", 
#'              kingdom  = "plant", 
#'              organism = "Arabidopsis thaliana", 
#'              path     = file.path("_ncbi_downloads","proteomes") )
#' 
#' 
#' file_path <- file.path("_ncbi_downloads","proteomes","Arabidopsis_thaliana_protein.faa.gz")
#' Ath_proteome <- read_proteome(file_path, format = "fasta")
#' 
#' 
#' }
#' @references 
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq}
#' 
#' \url{http://www.ncbi.nlm.nih.gov/refseq/about/}
#' 
#' @seealso \code{\link{read_proteome}}
#' @export

getProteome <- function(db = "refseq", kingdom, organism, path = file.path("_ncbi_downloads","proteomes")){
        
        if(!is.element(db,c("refseq")))
                stop("Please select one of the available data bases: 'refseq'")
        
        if(!is.genome.available(organism = organism))
                stop(paste0("Unfortunately for '",organism,"' no genome is stored on NCBI."))
        
        if(db == "refseq"){
                
                
                if(!file.exists(path)){
                        
                        dir.create(path, recursive = TRUE)
                        
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
                
                file_path <- file.path(path,paste0(organism,"_protein.faa.gz"))
                
                if(!file.exists(file_path)){
                        
                        download_url <- paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/",
                                               organism,"/latest_assembly_versions/",url_lates_version,"/",
                                               paste0(query_url_list_files,"_protein.faa.gz"))
                        
                        downloader::download(download_url, file_path,
                                             mode = "wb")
                        
                        
                        docFile( file.name = paste0(organism,"_proteome.faa.gz"),
                                 organism  = organism, 
                                 url       = download_url, 
                                 database  = db,
                                 path      = path)
                        
                        
                        # NCBI limits requests to three per second
                        Sys.sleep(0.33)
                        
                }
                
        }
        
        print(paste0("The proteome of '",organism,"' has been downloaded to '",path,"' and has been named '",paste0(organism,"_proteome.faa.gz"),"' ."))  
        
        
}








