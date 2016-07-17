#' @title Proteome Retrieval
#' @description This function retrieves a fasta-file storing the proteome of an organism of interest and stores
#' the proteome file in the folder '_ncbi_downloads/proteomes'.
#' @param db a character string specifying the database from which the proteome shall be retrieved: \code{refseq} or \code{genbank}.
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
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'  
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
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
#' # download the proteome of Arabidopsis thaliana from genbank
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' getProteome( db       = "genbank", 
#'              kingdom  = "plant", 
#'              organism = "Arabidopsis thaliana", 
#'              path     = file.path("_ncbi_downloads","proteomes") )
#' 
#' 
#' file_path <- file.path("_ncbi_downloads","proteomes","Arabidopsis_thaliana_protein.faa.gz")
#' Ath_proteome <- read_proteome(file_path, format = "fasta")
#' }
#' @references 
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq}
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank}
#' 
#' \url{http://www.ncbi.nlm.nih.gov/refseq/about/}
#' 
#' @seealso \code{\link{getGenome}}, \code{\link{getCDS}}, \code{\link{meta.retrieval}}, \code{\link{read_proteome}}
#' @export

getProteome <- function(db = "refseq", kingdom, organism, path = file.path("_ncbi_downloads","proteomes")){
        
        
        if(!is.element(db,c("refseq","genbank")))
                stop("Please select one of the available data bases: 'refseq' or 'genbank'")
        
        if(!is.genome.available(organism = organism))
                stop(paste0("Unfortunately for '",organism,"' no genome is stored on NCBI."))
        
                if(!file.exists(path)){
                        dir.create(path, recursive = TRUE)
                }
                
                subfolders <- getKingdoms()
                
                if(!is.element(kingdom,subfolders))
                        stop(paste0("Please select a valid kingdom: ",paste0(subfolders,collapse = ", ")))
                
                url_organisms <- try(RCurl::getURL(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/",db,"/",kingdom,"/"),
                                                   ftp.use.epsv = FALSE, dirlistonly = TRUE))
                
                # replace white space in scientific name with "_"
                # to match the corresponding folder on the NCBI server
                # e.g. "Arabodopsis thaliana" will become "Arabodopsis_thaliana"
                # organism <- stringr::str_replace(organism," ","_")
                
                check_organisms <- strsplit(url_organisms,"\n")
                check_organisms <- stringr::str_replace(unlist(check_organisms),"_"," ")
                check_organisms <- check_organisms[-which(is.element(check_organisms,c("assembly summary.txt", "check_organisms historical.txt")))]
                
                if(!is.element(organism,unlist(check_organisms)))
                        stop("Please choose a valid organism.")
                
                utils::download.file(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/",db,"/",kingdom,"/assembly_summary.txt"), 
                                     destfile = file.path(tempdir(),"assembly_summary.txt"))
                summary.file <- readr::read_tsv(file.path(tempdir(),"assembly_summary.txt"), comment = "#")
                
                colnames(summary.file) <- c("assembly_accession", "bioproject", "biosample",
                                            "wgs_master", "refseq_category", "taxid",
                                            "species_taxid", "organism_name", "infraspecific_name",
                                            "isolate", "version_status", "assembly_level",
                                            "release_type", "genome_rep", "seq_rel_date",
                                            "asm_name", "submitter", "gbrs_paired_asm",
                                            "paired_asm_comp", "ftp_path", "excluded_from_refseq")
                
                organism_name <- refseq_category <- version_status <- NULL
                query <- dplyr::filter(summary.file, stringr::str_detect(organism_name,organism), 
                                       ((refseq_category == "representative genome") || (refseq_category == "reference genome")), 
                                       (version_status == "latest"))
                
                if (nrow(query) > 1){
                        query <- query[1, ]
                }
                
                organism <- stringr::str_replace(organism," ","_")
                
                download_url <- paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/",db,"/",kingdom,"/",
                                       organism,"/latest_assembly_versions/",paste0(query$assembly_accession,"_",query$asm_name),"/",paste0(query$assembly_accession,"_",query$asm_name,"_protein.faa.gz"))
                
                if (nrow(query) == 1){
                        downloader::download(download_url, 
                                             destfile = file.path(path,paste0(organism,"_protein.faa.gz")), mode = "wb")
                        
                        docFile( file.name = paste0(organism,"_protein.faa.gz"),
                                 organism  = organism, 
                                 url       = download_url, 
                                 database  = db,
                                 path      = path)
                        
                        # NCBI limits requests to three per second
                        Sys.sleep(0.33)
                        
                        print(paste0("The genome of '",organism,"' has been downloaded to '",path,"' and has been named '",paste0(organism,"_protein.faa.gz"),"' ."))
                } else {
                        
                        warning ("File: ",download_url, " could not be loaded properly...")
        }
}






