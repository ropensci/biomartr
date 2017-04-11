#' @title Genome Annotation Retrieval
#' @description  Main retrieval function for GFF files of an 
#' organism of interest. By specifying the scientific name of an organism of 
#' interest the corresponding gff file storing the annotation  for the organism 
#' of interest can be downloaded and stored locally. GFF files can be retrieved 
#' from several databases.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' \item \code{db = "ensemblgenomes"}
#' } 
#' @param organism a character string specifying the scientific name of the 
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param path a character string specifying the location (a folder) in which 
#' the corresponding annotation file shall be stored. Default is 
#' \code{path = file.path("_ncbi_downloads","genomes")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'  
#' and creates a directory '_ncbi_downloads/annotation' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/annotation' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded annotation file.
#' @examples \dontrun{
#' # download the annotation of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/annotation'
#' getGFF( db       = "refseq", 
#'                organism = "Arabidopsis thaliana", 
#'                path = file.path("_ncbi_downloads","annotation"))
#' 
#' 
#' # download the genome of Arabidopsis thaliana from genbank
#' # and store the corresponding genome file in '_ncbi_downloads/annotation'
#' getGFF( db       = "genbank", 
#'                organism = "Arabidopsis thaliana", 
#'                path = file.path("_ncbi_downloads","annotation"))
#' 
#' }
#' 
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}}, 
#' \code{\link{getGenome}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}}
#' @export

getGFF <-
    function(db = "refseq",
             organism,
             path = file.path("_ncbi_downloads", "annotation")) {
       if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
            stop(
                "Please select one of the available data bases: 'refseq',
                'genbank', 'ensembl', 'ensemblgenomes'."
            )
        
        if (is.element(db, c("refseq", "genbank"))) {
            # get Kingdom Assembly Summary file
            AssemblyFilesAllKingdoms <-
                getKingdomAssemblySummary(db = db)
            
            # test wheter or not genome is available
            is.genome.available(organism = organism, db = db)
            
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            organism_name <-
                refseq_category <- version_status <- NULL
            
            organism <-
                stringr::str_replace_all(organism, "\\(", "")
            organism <-
                stringr::str_replace_all(organism, "\\)", "")
            
            
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
                message(
                    paste0(
                        "----------> No reference genome or representative 
                        genome was found for '",
                        organism,
                        "'. Thus, download for this species has been omitted."
                    )
                )
            } else {
                if (nrow(FoundOrganism) > 1) {
                    warnings(
                        "More than one entry has been found for '",
                        organism,
                        "'. Only the first entry '",
                        FoundOrganism[1, 1],
                        "' has been used for subsequent genome retrieval."
                    )
                    FoundOrganism <- FoundOrganism[1, ]
                }
                
                organism <-
                    stringr::str_replace_all(organism, " ", "_")
                
                download_url <-
                    paste0(FoundOrganism$ftp_path,
                           "/",
                           paste0(
                               basename(FoundOrganism$ftp_path),
                               "_genomic.gff.gz"
                           ))
                
                if (!exists.ftp.file(url = paste0(FoundOrganism$ftp_path, "/"),
                                     file.path = download_url)) {
                    message(
                      "Unfortunately no GFF file could be found for organism '",
                        organism,
                      "'. Thus, the download of this organism has been omitted."
                    )
                    return(FALSE)
                }
                
                # download_url <- paste0(query$ftp_path,query$`
                # assembly_accession`,"_",query$asm_name,"_genomic.fna.gz")
                local.org <-
                    stringr::str_replace_all(organism, "-", "_")
                local.org <-
                    stringr::str_replace_all(organism, "\\/", "_")
                
                if (nrow(FoundOrganism) == 1) {
                    if (file.exists(file.path(
                        path,
                        paste0(local.org, "_genomic_", db, ".gff.gz")
                    ))) {
                        message(
                            "File ",
                            file.path(
                                path,
                                paste0(local.org, "_genomic_", db, ".gff.gz")
                            ),
                            " exists already. Thus, download has been skipped."
                        )
                    } else {
                        tryCatch({
                            utils::capture.output(
                                custom_download(
                                    download_url,
                                    destfile = file.path(
                                        path,
                                        paste0(local.org, "_genomic_", db, 
                                               ".gff.gz")
                                    ),
                                    mode = "wb"
                                )
                            )
                        }, error = function(e)
                            stop(
                                "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' 
                                cannot be reached. Are you connected to the 
                                internet? Is the the FTP site '",
                                download_url,
                                "' currently available?",
                                call. = FALSE
                            ))
                    }
                    
                    
                    docFile(
                        file.name = paste0(local.org, "_genomic_", db, 
                                           ".gff.gz"),
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
                    
                    message(
                        paste0(
                            "The *.gff annotation file of '",
                            organism,
                            "' has been downloaded to '",
                            path,
                            "' and has been named '",
                            paste0(local.org, "_genomic_", db, ".gff.gz"),
                            "'."
                        )
                    )
                    
                    return(file.path(
                        path,
                        paste0(local.org, "_genomic_", db, ".gff.gz")
                    ))
                } else {
                    stop(
                        "File: ",
                        download_url,
                        " could not be loaded properly... Are you connected to 
                        the internet?"
                    )
                }
            }
        }
        
        if (db == "ensembl") {
            # create result folder
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            # download genome sequence from ENSEMBL
            genome.path <-
                getENSEMBL.Annotation(organism, type = "dna", 
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
                        "The *.gff annotation file of '",
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
                getENSEMBLGENOMES.Annotation(organism, type = "dna", 
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
                        "The *.gff annotation file of '",
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







