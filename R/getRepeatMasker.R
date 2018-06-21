#' @title Repeat Masker Retrieval
#' @description  Main Repeat Masker output retrieval function for an 
#' organism of interest.
#' By specifying the scientific name of an organism of interest the 
#' corresponding Repeat Masker file storing the genome of the organism of 
#' interest can be downloaded and stored locally. 
#' Repeat Masker files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' }
#' @param organism a character string specifying the scientific name of the 
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param path a character string specifying the location (a folder) in which 
#' the corresponding file shall be stored. Default is 
#' \code{path} = \code{file.path("_ncbi_downloads","repeatmasker")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'  
#' and creates a directory '_ncbi_downloads/repeatmasker' to store
#' the files of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/repeatmasker' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded Repeat Masker output file.
#' @examples \dontrun{
#' 
#' # download the Repeat Masker output file of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' file_path <- getRepeatMasker( db       = "refseq", 
#'              organism = "Arabidopsis thaliana", 
#'              path = file.path("_ncbi_downloads","repeatmasker"))
#' 
#' Ath_repeatmasker <- read_rm(file_path)
#' 
#' 
#' # download the Repeat Masker output file of Arabidopsis thaliana from genbank
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' file_path <- getRepeatMasker( db       = "genbank", 
#'              organism = "Arabidopsis thaliana", 
#'              path = file.path("_ncbi_downloads","repeatmasker"))
#' 
#' Ath_repeatmasker <- read_rm(file_path)
#' }
#' 
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}}, 
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}}, 
#' \code{\link{read_rm}}, \code{\link{getGenome}}
#' @export

getRepeatMasker <-
    function(db = "refseq",
             organism,
             reference = TRUE,
             path = file.path("_ncbi_downloads", "repeatmasker")) {
        
    if (!is.element(db, c("refseq", "genbank")))
            stop(
                "Please select one of the available data bases for which ",
                "Repeat Masker output files are available: 'refseq' or ",
                "'genbank'.",
                call. = FALSE
            )
        
        message("Starting Repeat Masker retrieval of '", organism, "' from ", db, " ...")
        message("\n")
        
        if (is.element(db, c("refseq", "genbank"))) {
            # get Kingdom Assembly Summary file
            AssemblyFilesAllKingdoms <-
                getKingdomAssemblySummary(db = db)
            
            # test wheter or not genome is available
            suppressMessages(is.genome.available(organism = organism, db = db))
            
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            organism_name <-
                refseq_category <- version_status <- NULL
            
            organism <-
                stringr::str_replace_all(organism, "\\(", "")
            organism <-
                stringr::str_replace_all(organism, "\\)", "")
            
            assembly_accession <- taxid <- NULL
            
            if (reference) {
                if (!is.taxid(organism)) {
                    FoundOrganism <-
                        dplyr::filter(
                            AssemblyFilesAllKingdoms,
                            stringr::str_detect(organism_name, organism) | 
                                stringr::str_detect(assembly_accession, organism),
                            ((refseq_category == "representative genome") |
                                 (refseq_category == "reference genome")
                            ),
                            (version_status == "latest")
                        ) 
                } else {
                    FoundOrganism <-
                        dplyr::filter(
                            AssemblyFilesAllKingdoms,
                            taxid == as.integer(organism),
                            ((refseq_category == "representative genome") |
                                 (refseq_category == "reference genome")
                            ),
                            (version_status == "latest"))
                }
            } else {
                if (!is.taxid(organism)) {
                    FoundOrganism <-
                        dplyr::filter(
                            AssemblyFilesAllKingdoms,
                            stringr::str_detect(organism_name, organism) |
                                stringr::str_detect(assembly_accession, organism),
                            (version_status == "latest")
                        ) 
                } else {
                    FoundOrganism <-
                        dplyr::filter(
                            AssemblyFilesAllKingdoms,
                            taxid == as.integer(organism),
                            (version_status == "latest")
                        ) 
                }
            }
            
            if (nrow(FoundOrganism) == 0) {
                    message(
                            paste0(
                                    "----------> No reference genome or representative genome was found for '",
                                    organism, "'. Thus, download for this species has been omitted.",
                                    " Have you tried to specify 'reference = FALSE' ?"
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
                        "' has been used for subsequent genome retrieval."
                    )
                    FoundOrganism <- FoundOrganism[1,]
                }
                
                organism <-
                    stringr::str_replace_all(organism, " ", "_")
                
                download_url <-
                    paste0(FoundOrganism$ftp_path,
                           "/",
                           paste0(
                               basename(FoundOrganism$ftp_path),
                               "_rm.out.gz"
                           ))
                
                # if (!exists.ftp.file(url = paste0(FoundOrganism$ftp_path, "/"),
                #                      file.path = download_url)) {
                #     message(
                #         "Unfortunately no genome file could be found for organism '",
                #         organism,
                #         "'. Thus, the download of this organism has been omitted."
                #     )
                #     return(FALSE)
                # }
                
                # download_url <- paste0(query$ftp_path,query$`
                # assembly_accession`,"_",query$asm_name,"_genomic.fna.gz")
                
                local.org <-
                    stringr::str_replace_all(organism, "-", "_")
                local.org <-
                    stringr::str_replace_all(organism, "\\/", "_")
                
                if (nrow(FoundOrganism) == 1) {
                    if (file.exists(file.path(
                        path,
                        paste0(local.org, "_rm_", db, ".out.gz")
                    ))) {
                        message(
                            "File ",
                            file.path(
                                path,
                                paste0(local.org, "_rm_", db, ".out.gz")
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
                                        paste0(local.org,
                                               "_rm_", db, ".out.gz")
                                    ),
                                    mode = "wb"
                                )
                            )
                            
                            message("RepeatMasker download is completed!")
                                
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
                                                         "_rm.out.gz"
                                                     )))$md5
                            
                            message("Checking md5 hash of file: ", 
                                    md5_file_path , " ...")
                            if (!(tools::md5sum(file.path(
                                path,
                                paste0(local.org, "_rm_", db, ".out.gz")
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
                        file.name = paste0(local.org, "_rm_", db, 
                                           ".out.gz"),
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
                    
                    doc <- tibble::tibble(
                        file_name = paste0(ifelse(is.taxid(organism), paste0("taxid_", local.org), local.org), "_genomic_", db,
                                           ".fna.gz"),
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
                    
                    readr::write_tsv(doc, path = file.path(path,paste0("doc_",local.org,"_db_",db,".tsv")))
                    
                    message(
                        paste0(
                            "The Repeat Masker output file of '",
                            organism,
                            "' has been downloaded to '",
                            path,
                            "' and has been named '",
                            paste0(local.org, "_rm_", db, ".out.gz"),
                            "'."
                        )
                    )
                    
                    return(file.path(
                        path,
                        paste0(local.org, "_rm_", db, ".out.gz")
                    ))
                } else {
                    stop(
                        "File: ",
                        download_url,
                        " could not be loaded properly... Are you connected to 
                        the internet?",
                        call. = FALSE
                    )
                }
            }
        }
}





