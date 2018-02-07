#' @title Genome Annotation Retrieval (GTF)
#' @description  Main retrieval function for GTF files of an 
#' organism of interest. By specifying the scientific name of an organism of 
#' interest the corresponding GTF file storing the annotation  for the organism 
#' of interest can be downloaded and stored locally. GTF files can be retrieved 
#' from several databases.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "ensembl"}
#' \item \code{db = "ensemblgenomes"}
#' } 
#' @param organism a character string specifying the scientific name of the 
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param path a character string specifying the location (a folder) in which 
#' the corresponding annotation file shall be stored. Default is 
#' \code{path = file.path("ensembl","annotation")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'  
#' and creates a directory 'ensembl/annotation' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' 'ensembl/annotation' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded annotation file.
#' @examples \dontrun{
#' # download the annotation of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in 'ensembl/annotation'
#' getGTF( db       = "ensembl", 
#'                organism = "Homo sapiens", 
#'                path = file.path("ensembl","annotation"))
#' 
#' }
#' 
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}}, 
#' \code{\link{getGenome}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}},
#' \code{\link{getGFF}} 
#' @export

getGTF <-
        function(db = "ensembl",
                 organism,
                 path = file.path("ensembl", "annotation")) {
                if (!is.element(db, c("ensembl", "ensemblgenomes")))
                        stop(
                                "Please select one of the available data bases: 
                                'ensembl' or 'ensemblgenomes'."
                        )
                
                message("Starting gtf retrieval of '", organism, "' from ", db, " ...")
                message("\n")
                
                if (db == "ensembl") {
                        # create result folder
                        if (!file.exists(path)) {
                                dir.create(path, recursive = TRUE)
                        }
                        
                        # download genome sequence from ENSEMBL
                        genome.path <-
                                getENSEMBL.gtf(organism, type = "dna", 
                                                      id.type = "toplevel", path)
                        
                        if (is.logical(genome.path)) {
                                invisible(return(TRUE))
                        } else {
                                new.organism <- stringr::str_replace_all(organism, " ", "_")
                                
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
                                                "The API 'http://rest.ensembl.org' does not seem to work
                                                properly. Are you connected to the internet? 
                                                Is the homepage '",
                                                json.qry.info,
                                                "' currently available?",
                                                call. = FALSE
                                        ))
                                
                                # generate Genome documentation
                                sink(file.path(
                                        path,
                                        paste0("doc_", new.organism, "_db_", db, ".txt")
                                ))
                                
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
                                cat(
                                        paste0(
                                                "genebuild_last_geneset_update: ",
                                                json.qry.info$genebuild_last_geneset_update
                                        )
                                )
                                cat("\n")
                                cat(paste0(
                                        "assembly_accession: ",
                                        json.qry.info$assembly_accession
                                ))
                                cat("\n")
                                cat(
                                        paste0(
                                                "genebuild_initial_release_date: ",
                                                json.qry.info$genebuild_initial_release_date
                                        )
                                )
                                
                                sink()
                                
                                message(
                                        paste0(
                                                "The *.gtf annotation file of '",
                                                organism,
                                                "' has been downloaded to '",
                                                genome.path,
                                                "' and has been named '",
                                                basename(genome.path),
                                                "'."
                                        )
                                )
                                
                                return(genome.path)
                        }
                }
                
                if (db == "ensemblgenomes") {
                        # create result folder
                        if (!file.exists(path)) {
                                dir.create(path, recursive = TRUE)
                        }
                        
                        # download genome sequence from ENSEMBLGENOMES
                        genome.path <-
                                getENSEMBLGENOMES.gtf(organism, type = "dna", 
                                                             id.type = "toplevel", path)
                        
                        if (is.logical(genome.path)) {
                                invisible(return(TRUE))
                        } else {
                                new.organism <- stringr::str_replace_all(organism, " ", "_")
                                
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
                                                "The API 'http://rest.ensemblgenomes.org' does not seem 
                                                to work properly. Are you connected to the internet? 
                                                Is the homepage 'http://rest.ensemblgenomes.org' 
                                                currently available?",
                                                call. = FALSE
                                        ))
                                
                                # generate Genome documentation
                                sink(file.path(
                                        path,
                                        paste0("doc_", new.organism, "_db_", db, ".txt")
                                ))
                                
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
                                cat(
                                        paste0(
                                                "genebuild_last_geneset_update: ",
                                                json.qry.info$genebuild_last_geneset_update
                                        )
                                )
                                cat("\n")
                                cat(paste0(
                                        "assembly_accession: ",
                                        json.qry.info$assembly_accession
                                ))
                                cat("\n")
                                cat(
                                        paste0(
                                                "genebuild_initial_release_date: ",
                                                json.qry.info$genebuild_initial_release_date
                                        )
                                )
                                
                                sink()
                                
                                message(
                                        paste0(
                                                "The *.gtf annotation file of '",
                                                organism,
                                                "' has been downloaded to '",
                                                genome.path,
                                                "' and has been named '",
                                                basename(genome.path),
                                                "'."
                                        )
                                )
                                
                                return(genome.path)
                        }
                }
}







