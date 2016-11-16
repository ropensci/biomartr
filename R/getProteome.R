#' @title Proteome Retrieval
#' @description Main proteome retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest the corresponding fasta-file storing the proteome of the organism of interest
#' can be downloaded and stored locally. Proteome files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"}, \code{db = "ensembl"} or \code{db = "ensemblgenomes"}.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. \code{organism = "Homo sapiens"}.
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
#' @return File path to downloaded proteome.
#' @examples \dontrun{
#' 
#' # download the proteome of Arabidopsis thaliana from refseq
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' file_path <- getProteome( db       = "refseq", 
#'              organism = "Arabidopsis thaliana", 
#'              path     = file.path("_ncbi_downloads","proteomes") )
#' 
#' Ath_proteome <- read_proteome(file_path, format = "fasta")
#' 
#' # download the proteome of Arabidopsis thaliana from genbank
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' file_path <- getProteome( db       = "genbank", 
#'              organism = "Arabidopsis thaliana", 
#'              path     = file.path("_ncbi_downloads","proteomes") )
#' 
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
#' @seealso \code{\link{getGenome}}, \code{\link{getCDS}}, \code{\link{getGFF}}, \code{\link{meta.retrieval}}, \code{\link{read_proteome}}
#' @export

getProteome <- function(db = "refseq", organism, path = file.path("_ncbi_downloads","proteomes")){
    
    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop("Please select one of the available data bases: 'refseq', 'genbank', 'ensembl' or 'ensemblgenomes'.", call. = FALSE)
    
    if (is.element(db, c("refseq", "genbank"))) {
        # get Kingdom Assembly Summary file
        AssemblyFilesAllKingdoms <- getKingdomAssemblySummary(db = db)
        
        # test wheter or not genome is available
        is.genome.available(organism = organism, db = db)
        
        if (!file.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
        
        organism_name <- refseq_category <- version_status <- NULL
        organism <- stringr::str_replace_all(organism,"\\(","")
        organism <- stringr::str_replace_all(organism,"\\)","")
        
        FoundOrganism <-
            dplyr::filter(
                AssemblyFilesAllKingdoms,
                stringr::str_detect(organism_name, organism),
                ((refseq_category == "representative genome") ||
                     (refseq_category == "reference genome")
                ),
                (version_status == "latest")
            )
        
        if (nrow(FoundOrganism) == 0) {
            cat("\n")
            cat(paste0("----------> No reference genome or representative genome was found for '",organism,"'. Thus, download for this species has been omitted."))
            cat("\n")
        } else {
            if (nrow(FoundOrganism) > 1) {
                warnings(
                    "More than one entry has been found for '",
                    organism,
                    "'. Only the first entry '",
                    FoundOrganism[1, 1],
                    "' has been used for subsequent proteome retrieval."
                )
                FoundOrganism <- FoundOrganism[1,]
            }
            
            organism <- stringr::str_replace_all(organism, " ", "_")
            
            download_url <-
                paste0(
                    FoundOrganism$ftp_path,
                    "/",
                    paste0(
                        basename(FoundOrganism$ftp_path),
                        "_protein.faa.gz"
                    )
                )
            
            local.org <- stringr::str_replace_all(organism,"-","_")
            local.org <- stringr::str_replace_all(organism,"\\/","_")
            
            if (nrow(FoundOrganism) == 1) {
                tryCatch({
                    utils::capture.output(downloader::download(
                        download_url,
                        destfile = file.path(path, paste0(
                            local.org, "_protein_", db, ".faa.gz"
                        )),
                        mode = "wb"
                    ))
                }, error = function(e)
                    stop(
                        "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' cannot be reached. Are you connected to the internet? Is the the FTP site '",
                        download_url,
                        "' currently available?",
                        call. = FALSE
                    ))
                
                docFile(
                    file.name = paste0(local.org, "_protein.faa.gz"),
                    organism  = organism,
                    url       = download_url,
                    database  = db,
                    path      = path,
                    refseq_category = FoundOrganism$refseq_category,
                    assembly_accession = FoundOrganism$assembly_accession,
                    bioproject = FoundOrganism$bioproject,
                    biosample = FoundOrganism$biosample,
                    taxid = FoundOrganism$taxid,
                    infraspecific_name = FoundOrganism$infraspecific_name,
                    version_status = FoundOrganism$version_status,
                    release_type = FoundOrganism$release_type,
                    genome_rep = FoundOrganism$genome_rep,
                    seq_rel_date = FoundOrganism$seq_rel_date,
                    submitter = FoundOrganism$submitter
                )
                
                # NCBI limits requests to three per second
                Sys.sleep(0.33)
                
                
                print(
                    paste0(
                        "The proteome of '",
                        organism,
                        "' has been downloaded to '",
                        path,
                        "' and has been named '",
                        paste0(local.org, "_protein_", db, ".faa.gz"),
                        "' ."
                    )
                )
                
                return(file.path(path, paste0(
                    local.org, "_protein_", db, ".faa.gz"
                )))
            } else {
                stop(
                    "File: ",
                    download_url,
                    " could not be loaded properly... Are you connected to the internet?", call. = FALSE
                )
            }
        }
    }
    
    if (db == "ensembl") {
        
        # create result folder
        if (!file.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
        
        # download proteome sequence from ENSEMBL
        proteome.path <- getENSEMBL.Seq(organism, type = "pep", id.type = "all", path)
        
        if (is.logical(proteome.path)) {
            invisible(return(TRUE))
        } else {
            new.organism <- stringr::str_replace_all(organism," ","_")
            
            # test proper API access
            tryCatch({
                json.qry.info <-
                    jsonlite::fromJSON(
                        paste0(
                            "http://rest.ensembl.org/info/assembly/",
                            new.organism,
                            "?content-type=application/json"
                        )
                    )
            }, error = function(e)
                stop(
                    "The API 'http://rest.ensembl.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensembl.org' currently available?", call. = FALSE
                ))
            
            cwd <- getwd()
            
            setwd(path)
            
            # generate Proteome documentation
            sink(paste0("doc_",new.organism,"_db_",db,".txt"))
            
            cat(paste0("File Name: ", proteome.path))
            cat("\n")
            cat(paste0("Organism Name: ", new.organism))
            cat("\n")
            cat(paste0("Database: ", db))
            cat("\n")
            cat(paste0("Download_Date: ", date()))
            cat("\n")
            cat(paste0("assembly_name: ", json.qry.info$assembly_name))
            cat("\n")
            cat(paste0("assembly_date: ", json.qry.info$assembly_date))
            cat("\n")
            cat(paste0("genebuild_last_geneset_update: ", json.qry.info$genebuild_last_geneset_update))
            cat("\n")
            cat(paste0("assembly_accession: ", json.qry.info$assembly_accession))
            cat("\n")
            cat(paste0("genebuild_initial_release_date: ", json.qry.info$genebuild_initial_release_date))
            
            sink()
            
            setwd(cwd)
            
            print(
                paste0(
                    "The proteome of '",
                    organism,
                    "' has been downloaded to '",
                    path,
                    "' and has been named '",
                    basename(proteome.path),
                    "'."
                )
            )
            
            return(proteome.path)
        }
    }
    
    if (db == "ensemblgenomes") {
        
        # create result folder
        if (!file.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
        
        # download proteome sequence from ENSEMBLGENOMES
        proteome.path <- getENSEMBLGENOMES.Seq(organism, type = "pep", id.type = "all", path)
        
        if (is.logical(proteome.path)) {
            invisible(return(TRUE))
        } else {
            new.organism <- stringr::str_replace_all(organism," ","_")
            
            # test proper API access
            tryCatch({
                json.qry.info <-
                    jsonlite::fromJSON(
                        paste0(
                            "http://rest.ensemblgenomes.org/info/assembly/",
                            new.organism,
                            "?content-type=application/json"
                        )
                    )
            }, error = function(e)
                stop(
                    "The API 'http://rest.ensemblgenomes.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensemblgenomes.org' currently available?", call. = FALSE
                ))
            
            cwd <- getwd()
            
            setwd(path)
            
            # generate Proteome documentation
            sink(paste0("doc_",new.organism,"_db_",db,".txt"))
            
            cat(paste0("File Name: ", proteome.path))
            cat("\n")
            cat(paste0("Organism Name: ", new.organism))
            cat("\n")
            cat(paste0("Database: ", db))
            cat("\n")
            cat(paste0("Download_Date: ", date()))
            cat("\n")
            cat(paste0("assembly_name: ", json.qry.info$assembly_name))
            cat("\n")
            cat(paste0("assembly_date: ", json.qry.info$assembly_date))
            cat("\n")
            cat(paste0("genebuild_last_geneset_update: ", json.qry.info$genebuild_last_geneset_update))
            cat("\n")
            cat(paste0("assembly_accession: ", json.qry.info$assembly_accession))
            cat("\n")
            cat(paste0("genebuild_initial_release_date: ", json.qry.info$genebuild_initial_release_date))
            
            sink()
            
            setwd(cwd)
            
            print(
                paste0(
                    "The proteome of '",
                    organism,
                    "' has been downloaded to '",
                    path,
                    "' and has been named '",
                    basename(proteome.path),
                    "'."
                )
            )
            
            return(proteome.path)
        }
    }
}






