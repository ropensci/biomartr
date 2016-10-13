#' @title Genome Retrieval
#' @description This function retrieves a fasta-file storing the genome of an organism of interest and stores
#' the genome file in the folder '_ncbi_downloads/genomes'.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{refseq} or \code{genbank}.
#' Right now only the ref seq database is included. Later version of \pkg{biomartr} will also allow
#' sequence retrieval from additional databases.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis thaliana'.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' genome shall be stored. Default is \code{path} = \code{file.path("_ncbi_downloads","genomes")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
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
#' file_path <- getGenome( db       = "refseq", 
#'              organism = "Arabidopsis thaliana", 
#'              path = file.path("_ncbi_downloads","genomes"))
#' 
#' Ath_genome <- read_genome(file_path, format = "fasta")
#' 
#' 
#' # download the genome of Arabidopsis thaliana from genbank
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' file_path <- getGenome( db       = "genbank", 
#'              organism = "Arabidopsis thaliana", 
#'              path = file.path("_ncbi_downloads","genomes"))
#' 
#' Ath_genome <- read_genome(file_path, format = "fasta")
#' }
#' @references 
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq}
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank}
#' 
#' \url{http://www.ncbi.nlm.nih.gov/refseq/about/}
#' 
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{meta.retrieval}}, \code{\link{read_genome}}
#' @export

getGenome <-
    function(db = "refseq",
             organism,
             path = file.path("_ncbi_downloads", "genomes")) {
        
        if (!is.element(db, c("refseq", "genbank","ensembl")))
            stop("Please select one of the available data bases: 'refseq', 'genbank', or 'ensembl'.")
        
        if (is.element(db, c("refseq", "genbank"))) {
            # get Kingdom Assembly Summary file
            AssemblyFilesAllKingdoms <- getKingdomAssemblySummary(db = db)
        
            # test wheter or not genome is available
            is.genome.available(organism = organism, db = db)
            
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            organism_name <-
                refseq_category <- version_status <- NULL
            
            FoundOrganism <-
                dplyr::filter(
                    AssemblyFilesAllKingdoms,
                    stringr::str_detect(organism_name, organism),
                    ((refseq_category == "representative genome") ||
                         (refseq_category == "reference genome")
                    ),
                    (version_status == "latest")
                )
            
            if (nrow(FoundOrganism) > 1) {
                warnings(
                    "More than one entry has been found for '",
                    organism,
                    "'. Only the first entry '",
                    FoundOrganism[1, 1],
                    "' has been used for subsequent genome retrieval."
                )
                FoundOrganism <- FoundOrganism[1,]
            }
            
            organism <- stringr::str_replace(organism, " ", "_")
            
            download_url <-
                paste0(
                    FoundOrganism$ftp_path,
                    "/",
                    paste0(
                        FoundOrganism$assembly_accession,
                        "_",
                        FoundOrganism$asm_name,
                        "_genomic.fna.gz"
                    )
                )
            
            # download_url <- paste0(query$ftp_path,query$`# assembly_accession`,"_",query$asm_name,"_genomic.fna.gz")
            
            if (nrow(FoundOrganism) == 1) {
                utils::capture.output(downloader::download(
                    download_url,
                    destfile = file.path(path, paste0(organism, "_genomic.fna.gz")),
                    mode = "wb"
                ))
                
                docFile(
                    file.name = paste0(organism, "_genomic.fna.gz"),
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
                        "The genome of '",
                        organism,
                        "' has been downloaded to '",
                        path,
                        "' and has been named '",
                        paste0(organism, "_genomic.fna.gz"),
                        "' ."
                    )
                )
                
                return(file.path(path, paste0(organism, "_genomic.fna.gz")))
            } else {
                warning (
                    "File: ",
                    download_url,
                    " could not be loaded properly... Are you connected to the internet?"
                )
            }
        }
    
        if (db == "ensembl") {
            
            # create result folder
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            # download genome sequence from ENSEMBL
            genome.path <- getENSEMBL.Seq(organism, type = "dna", id.type = "toplevel", path)
            
            new.organism <- stringr::str_replace(organism," ","_")
            
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
                    "The API 'http://rest.ensembl.org' does not seem to work properly. Are you connected to the internet? Is the homepage 'http://rest.ensembl.org' currently available?"
                ))
            
            cwd <- getwd()
            
            setwd(path)
            
            # generate Genome documentation
            sink(paste0("doc_",new.organism,"_db_",db,".txt"))
            
            cat(paste0("File Name: ", genome.path))
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
            
            return(genome.path)
        }
    
    }







