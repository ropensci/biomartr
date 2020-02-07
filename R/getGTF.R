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
#' } 
#' @param organism a character string specifying the scientific name of the 
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param remove_annotation_outliers shall outlier lines be removed from the input \code{annotation_file}? 
#' If yes, then the initial \code{annotation_file} will be overwritten and the removed outlier lines will be stored at \code{\link{tempdir}}
#' for further exploration.
#' @param path a character string specifying the location (a folder) in which 
#' the corresponding annotation file shall be stored. Default is 
#' \code{path = file.path("ensembl","annotation")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from ENSEMBL:
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
#' \code{\link{getGenome}}, \code{\link{getRNA}}, \code{\link{getRepeatMasker}}, 
#' \code{\link{getAssemblyStats}}, \code{\link{meta.retrieval}},
#' \code{\link{getGFF}} 
#' @export

getGTF <-
        function(db = "ensembl",
                 organism,
                 remove_annotation_outliers = FALSE,
                 path = file.path("ensembl", "annotation")) {
                if (!is.element(db, c("ensembl")))
                        stop(
                                "Please select one of the available data bases: db = 'ensembl'.", call. = FALSE
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
                            if (!genome.path)
                                return(FALSE)
                        } else {
                            
                            taxon_id <- assembly <- name <- accession <- NULL
                            
                            ensembl_summary <-
                                suppressMessages(is.genome.available(
                                    organism = organism,
                                    db = "ensembl",
                                    details = TRUE
                                ))
                            
                            if (nrow(ensembl_summary) > 1) {
                                if (is.taxid(organism)) {
                                    ensembl_summary <-
                                        dplyr::filter(ensembl_summary, taxon_id == as.integer(organism), !is.na(assembly))
                                } else {
                                    
                                    ensembl_summary <-
                                        dplyr::filter(
                                            ensembl_summary,
                                            (name == stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))) |
                                                (accession == organism), !is.na(assembly)
                                        )
                                }
                            }
                            
                            new.organism <- ensembl_summary$name[1]
                            new.organism <-
                                paste0(
                                    stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
                                    stringr::str_sub(new.organism, 2, nchar(new.organism))
                                ) 
                            
                            rest_url <- paste0(
                                "http://rest.ensembl.org/info/assembly/",
                                new.organism,
                                "?content-type=application/json"
                            )
                            
                            if (curl::curl_fetch_memory(rest_url)$status_code != 200) {
                                message(
                                    "The url: '",rest_url,"' cannot be reached. This might be due to a connection issue or incorrect url path (e.g. not valid organism name).")
                                return(FALSE)
                            }
                            
                            # test proper API access
                            json.qry.info <-
                                jsonlite::fromJSON(rest_url)
                            
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
                            
                            doc <- tibble::tibble(
                                file_name = genome.path,
                                organism = new.organism,
                                database = db,
                                download_data = date(),
                                assembly_name = ifelse(!is.null(json.qry.info$assembly_name), json.qry.info$assembly_name, "none"),
                                assembly_date = ifelse(!is.null(json.qry.info$assembly_date), json.qry.info$assembly_date, "none"),
                                genebuild_last_geneset_update = ifelse(!is.null(json.qry.info$genebuild_last_geneset_update), json.qry.info$genebuild_last_geneset_update, "none"), 
                                assembly_accession = ifelse(!is.null(json.qry.info$assembly_accession), json.qry.info$assembly_accession, "none"),
                                genebuild_initial_release_date = ifelse(!is.null(json.qry.info$genebuild_initial_release_date), json.qry.info$genebuild_initial_release_date, "none")
                                
                            )
                            
                            readr::write_tsv(doc, file.path(
                                path,
                                paste0("doc_", new.organism, "_db_", db, ".tsv"))
                            )
                            
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
                            
                            output_path <- check_annotation_biomartr(genome.path, remove_annotation_outliers = remove_annotation_outliers)
                            
                            return(output_path)
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
                            if (!genome.path)
                                return(FALSE)
                        } else {
                            
                            taxon_id <- assembly <- name <- accession <- NULL
                            
                            ensembl_summary <-
                                suppressMessages(is.genome.available(
                                    organism = organism,
                                    db = "ensemblgenomes",
                                    details = TRUE
                                ))
                            
                            if (nrow(ensembl_summary) > 1) {
                                if (is.taxid(organism)) {
                                    ensembl_summary <-
                                        dplyr::filter(ensembl_summary, taxon_id == as.integer(organism), !is.na(assembly))
                                } else {
                                    
                                    ensembl_summary <-
                                        dplyr::filter(
                                            ensembl_summary,
                                            (name == stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))) |
                                                (accession == organism), !is.na(assembly)
                                        )
                                }
                            }
                            
                            new.organism <- ensembl_summary$name[1]
                            new.organism <-
                                paste0(
                                    stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
                                    stringr::str_sub(new.organism, 2, nchar(new.organism))
                                ) 
                            
                            rest_url <- paste0(
                                "http://rest.ensembl.org/info/assembly/",
                                new.organism,
                                "?content-type=application/json"
                            )
                            
                            if (curl::curl_fetch_memory(rest_url)$status_code != 200) {
                                message(
                                    "The url: '",rest_url,"' cannot be reached. This might be due to a connection issue or incorrect url path (e.g. not valid organism name).")
                                return(FALSE)
                            }
                            
                            # test proper API access
                            json.qry.info <-
                                jsonlite::fromJSON(rest_url)
                            
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
                            
                            doc <- tibble::tibble(
                                file_name = genome.path,
                                organism = new.organism,
                                database = db,
                                download_data = date(),
                                assembly_name = ifelse(!is.null(json.qry.info$assembly_name), json.qry.info$assembly_name, "none"),
                                assembly_date = ifelse(!is.null(json.qry.info$assembly_date), json.qry.info$assembly_date, "none"),
                                genebuild_last_geneset_update = ifelse(!is.null(json.qry.info$genebuild_last_geneset_update), json.qry.info$genebuild_last_geneset_update, "none"),
                                assembly_accession = ifelse(!is.null(json.qry.info$assembly_accession), json.qry.info$assembly_accession, "none"), 
                                genebuild_initial_release_date = ifelse(!is.null(json.qry.info$genebuild_initial_release_date), json.qry.info$genebuild_initial_release_date, "none")
                                
                            )
                            
                            readr::write_tsv(doc, file.path(
                                path,
                                paste0("doc_", new.organism, "_db_", db, ".tsv"))
                            )
                            
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







