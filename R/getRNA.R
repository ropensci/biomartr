#' @title RNA Sequence Retrieval
#' @description Main retrieval function for RNA sequences of an organism 
#' of interest. By specifying the scientific name of an organism of interest the
#' corresponding fasta-file storing the RNA information for the organism 
#' of interest can be downloaded and stored locally. 
#' RNA files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"}, 
#' \code{db = "ensembl"} or \code{db = "ensemblgenomes"}.
#' @param organism a character string specifying the scientific name of the 
#' organism of interest,\code{organism = "Homo sapiens"}.
#' @param path a character string specifying the location (a folder) in which 
#' the corresponding
#' CDS file shall be stored. Default is 
#' \code{path} = \code{file.path("_ncbi_downloads","RNA")}.
#' @author Hajk-Georg Drost
#' @return File path to downloaded RNA file.
#' @examples 
#' \dontrun{
#' # download the RNA of Arabidopsis thaliana from refseq
#' # and store the corresponding RNA file in '_ncbi_downloads/RNA'
#' file_path <- getRNA( db       = "refseq", 
#'              organism = "Arabidopsis thaliana", 
#'              path     = file.path("_ncbi_downloads","RNA"))
#' 
#' Ath_RNA <- read_rna(file_path, format = "fasta")
#' }
#' @seealso \code{\link{getGenome}}, \code{\link{getProteome}}, 
#' \code{\link{getGTF}}, \code{\link{getGFF}}, \code{\link{meta.retrieval}}, 
#' \code{\link{read_cds}}, \code{\link{getCDS}}
#' @export

getRNA <-
    function(db = "refseq",
             organism,
             path = file.path("_ncbi_downloads", "RNA")) {
        if (!is.element(db, c("refseq", "genbank", "ensembl", 
                              "ensemblgenomes")))
            stop(
                "Please select one of the available data bases: 
                'refseq', 'genbank', 'ensembl' or 'ensemblgenomes'.",
                call. = FALSE
            )
            
        message("Starting RNA retrieval of ", organism, " ...")
            
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
            
            FoundOrganism <- dplyr::filter(
                AssemblyFilesAllKingdoms,
                stringr::str_detect(organism_name, organism),
                ((refseq_category == "representative genome") |
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
                return("Not available")
            } else {
                if (nrow(FoundOrganism) > 1) {
                    warnings(
                        "More than one entry has been found for '",
                        organism,
                        "'. Only the first entry '",
                        FoundOrganism[1, 1],
                        "' has been used for subsequent genomic RNA retrieval."
                    )
                    FoundOrganism <- FoundOrganism[1,]
                }
                
                organism <-
                    stringr::str_replace_all(organism, " ", "_")
                
                download_url <-
                    paste0(
                        FoundOrganism$ftp_path,
                        "/",
                        paste0(
                            basename(FoundOrganism$ftp_path),
                            "_rna_from_genomic.fna.gz"
                        )
                    )
                
                local.org <-
                    stringr::str_replace_all(organism, "-", "_")
                local.org <-
                    stringr::str_replace_all(organism, "\\/", "_")
                
                if (!exists.ftp.file(url = paste0(FoundOrganism$ftp_path, "/"),
                                     file.path = download_url)) {
                    message(
                        "Unfortunately no RNA file could be found for 
                        organism '",
                        organism,
                        "'. Thus, the download of this organism has been 
                        omitted."
                    )
                    return(FALSE)
                }
                
                if (nrow(FoundOrganism) == 1) {
                    if (file.exists(file.path(
                        path,
                        paste0(
                            local.org,
                            "_rna_from_genomic_",
                            db,
                            ".fna.gz"
                        )
                    ))) {
                        message(
                            "File ",
                            file.path(
                                path,
                                paste0(
                                    local.org,
                                    "_rna_from_genomic_",
                                    db,
                                    ".fna.gz"
                                )
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
                                        paste0(
                                            local.org,
                                            "_rna_from_genomic_",
                                            db,
                                            ".fna.gz"
                                        )
                                    ),
                                    mode = "wb"
                                )
                            )
                            
                            # download md5checksum file for organism of interest
                            custom_download(
                             paste0(FoundOrganism$ftp_path,"/md5checksums.txt"),
                                file.path(path, 
                                  paste0(local.org, "_md5checksums.txt")),
                                mode = "wb"
                            )
                            
                            # test check sum
                            md5_file_path <- file.path(path, 
                                                       paste0(local.org, 
                                                         "_md5checksums.txt"))
                            md5_file <-
                                read_md5file(md5_file_path)
                            
                            file_name <- NULL
                            
                            md5_sum <- dplyr::filter(md5_file,
                                            file_name == paste0("./", paste0(
                                             basename(FoundOrganism$ftp_path),
                                                    "_rna_from_genomic.fna.gz"
                                                     )))$md5
                            
                            message("Checking md5 hash of file: ", 
                                    md5_file_path , " ...")
                            
                            if (!(tools::md5sum(file.path(
                                path,
                        paste0(local.org, "_rna_from_genomic_", db, ".fna.gz")
                            )) == md5_sum))
                                stop(
                                    paste0(
                                        "Please download the file '",
                                        md5_file_path,
            "' again. The md5 hash between the downloaded file and the file ",
                                        "stored at NCBI do not match.",
                                        collapse = ""
                                    )
                                )
                            unlink(md5_file_path)
                message("The md5 hash of file '", md5_file_path, "' matches!")
                        }, error = function(e) {
                            warning(
                                "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' 
                                cannot be reached. Are you connected to the 
                                internet? Is the the FTP site '",
                                download_url,
                                "' currently available?",
                                call. = FALSE
                            )
                            return(FALSE)
                        })
                    }
                    
                    docFile(
                        file.name = paste0(
                            local.org,
                            "_rna_from_genomic_",
                            db,
                            ".fna.gz"
                        ),
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
                            "The genomic RNA of '",
                            organism,
                            "' has been downloaded to '",
                            path,
                            "' and has been named '",
                            paste0(
                                local.org,
                                "_rna_from_genomic_",
                                db,
                                ".fna.gz"
                            ),
                            "' ."
                        )
                    )
                    
                    return(file.path(
                        path,
                        paste0(
                            local.org,
                            "_rna_from_genomic_",
                            db,
                            ".fna.gz"
                        )
                    ))
                } else {
                    warning(
                        "File: ",
                        download_url,
                        " could not be loaded properly... 
                        Are you connected to the internet?",
                        call. = FALSE
                    )
                }
            }
        }
        
        if (db == "ensembl") {
            # create result folder
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            # download CDS sequence from ENSEMBL
            rna.path <-
                getENSEMBL.Seq(organism, type = "ncrna", id.type = "none", path)
            
            if (is.logical(rna.path)) {
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
                }, error = function(e) {
                    warning(
                        "The API 'http://rest.ensembl.org' does not seem to work
                        properly. Are you connected to the internet? Is the 
                        homepage 'http://rest.ensembl.org' currently 
                        available?",
                        call. = FALSE
                    )
                    return(FALSE)
                })
                
                # generate CDS documentation
                sink(file.path(
                    path,
                    paste0("doc_", new.organism, "_db_", db, ".txt")
                ))
                
                cat(paste0("File Name: ", rna.path))
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
                        "The RNA of '",
                        organism,
                        "' has been downloaded to '",
                        path,
                        "' and has been named '",
                        basename(rna.path),
                        "'."
                    )
                )
                
                return(rna.path)
            }
        }
        
        if (db == "ensemblgenomes") {
            # create result folder
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            # download CDS sequence from ENSEMBLGENOMES
            rna.path <-
                getENSEMBLGENOMES.Seq(organism, type = "ncrna", 
                                      id.type = "none", path)
            
            if (is.logical(rna.path)) {
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
                }, error = function(e) {
                    warning(
                        "The API 'http://rest.ensemblgenomes.org' does not seem 
                        to work properly. Are you connected to the internet? 
                        Is the homepage 'http://rest.ensemblgenomes.org' 
                        currently available?",
                        call. = FALSE
                    )
                    return(FALSE)
                })
                
                # generate CDS documentation
                sink(file.path(
                    path,
                    paste0("doc_", new.organism, "_db_", db, ".txt")
                ))
                
                cat(paste0("File Name: ", rna.path))
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
                        "The RNA of '",
                        organism,
                        "' has been downloaded to '",
                        path,
                        "' and has been named '",
                        basename(rna.path),
                        "'."
                    )
                )
                
                return(rna.path)
            }
            
        }
    }








