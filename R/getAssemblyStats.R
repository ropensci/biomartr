#' @title Genome Assembly Stats Retrieval
#' @description  Main genome assembly stats retrieval function for an organism
#' of interest. By specifying the scientific name of an organism of interest the
#' corresponding  genome assembly stats file storing the assembly statistics of 
#' the organism of interest can be downloaded and stored locally. 
#' Genome assembly stats files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param organism a character string specifying the scientific name of the 
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param type shall only the file be retrieved (default) 
#' \code{type = "download"} or should the corresponding file be downloaded and 
#' subsequently be imported \code{type = "import"}.
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param path a character string specifying the location (a folder) in
#' which the corresponding file shall be stored. Default is 
#' \code{path} = \code{file.path("_ncbi_downloads","genomeassembly_stats")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'  
#' to retrieve available scientific names of organisms and creates a directory
#' '_ncbi_downloads/genomeassembly_stats' to store
#' the Genome Assembly Stats of interest as text file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomeassembly_stats' folder and is
#' accessible within the workspace, no download process will be performed.
#' 
#' An example genome assembly stats file can be found here:
#' ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/001/405/
#' GCF_000001405.36_GRCh38.p10/GCF_000001405.36_GRCh38.p10_assembly_stats.txt.
#' 
#' @return File path to downloaded genome assembly stats file.
#' @examples \dontrun{
#' # download the genome assembly stats file of Saccharomyces cerevisiae
#' # from NCBI RefSeq
#' # and store the corresponding genome file in 
#' # '_ncbi_downloads/genomeassembly_stats'
#' file_path <- getAssemblyStats( db = "refseq", 
#'                  organism = "Saccharomyces cerevisiae", 
#'                  path = file.path("_ncbi_downloads","genomeassembly_stats"))
#' # import the raw file as it is downloaded
#' Scerevisiae.stats <- read_assemblystats(file_path, type = "raw")
#' 
#' # download the genome assembly stats file of Saccharomyces cerevisiae
#' # from NCBI RefSeq 
#' # and import overall statistics of the genome assembly
#' Scerevisiae.stats.import <- getAssemblyStats( db = "refseq", 
#'                  organism = "Saccharomyces cerevisiae",
#'                  type = "import", 
#'                  path = file.path("_ncbi_downloads","genomeassembly_stats"))
#' }
#' 
#' @seealso \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}}, 
#' \code{\link{read_assemblystats}}
#' @export

getAssemblyStats <-
    function(db = "refseq",
             organism,
             reference = FALSE,
             type = "download",
             path = file.path("_ncbi_downloads", "genomeassembly_stats")) {
        
        
        if (!is.element(db, c("refseq", "genbank")))
            stop(paste0("Please select one of the available data bases: ",
                 "'refseq' and 'genbank'."),
                 call. = FALSE)
        
        if (!is.element(type, c("download", "import")))
            stop(
                paste0("Please choose either type = 'download' ",
                       "(if you would like to ",
                       "download the genome assembly stats file) or ",
               "type = 'import' (if you would like to download and import the ",
                       "genome assembly stats file).", collapse = ""),
                call. = FALSE
            )
        
        message("Starting assembly quality stats retrieval of '", organism, "' from ", db, " ...")
        message("\n")
        
        # get Kingdom Assembly Summary file
        AssemblyFilesAllKingdoms <-
            getKingdomAssemblySummary(db = db)
        
        species <- organism
        # test wheter or not genome is available
        suppressMessages(is.genome.available(organism = organism, db = db))
        
        if (!file.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
        
        organism_name <-
            refseq_category <- version_status <- NULL
        
        organism <- stringr::str_replace_all(organism, "\\(", "")
        organism <- stringr::str_replace_all(organism, "\\)", "")
        
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
                FoundOrganism <- FoundOrganism[1, ]
            }
            
            organism <-
                stringr::str_replace_all(organism, " ", "_")
            
            download_url <-
                paste0(FoundOrganism$ftp_path,
                       "/",
                       paste0(
                           basename(FoundOrganism$ftp_path),
                           "_assembly_stats.txt"
                       ))
            
            local.org <-
                stringr::str_replace_all(organism, "-", "_")
            local.org <-
                stringr::str_replace_all(organism, "\\/", "_")
            
            # if (!exists.ftp.file(url = paste0(FoundOrganism$ftp_path, "/"),
            #                      file.path = download_url)) {
            #     message(
            #         "Unfortunately no assembly stats file could be 
            #         found for organism '",
            #         organism,
            #         "'. Thus, the download of this organism has been omitted."
            #     )
            #     return(FALSE)
            # }
            
            if (nrow(FoundOrganism) == 1) {
                if (file.exists(file.path(
                    path,
                    paste0(local.org, "_assembly_stats_", db, ".txt")
                ))) {
                    message(
                        "File ",
                        file.path(
                            path,
                            paste0(local.org, "_assembly_stats_", db, ".txt")
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
                                    paste0(local.org, "_assembly_stats_", db, 
                                           ".txt")
                                ),
                                mode = "wb"
                            )
                        )
                           
                        message("Genome assembly quality stats file download completed!")
                        
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
                                                 file_name == paste0(" ./", paste0(
                                                         basename(FoundOrganism$ftp_path),
                                                         "_assembly_stats.txt"
                                                 )))$md5
                        
                        message("Checking md5 hash of file: ", 
                                md5_file_path , " ...")
                        
                        if (!(tools::md5sum(file.path(
                                path,
                                paste0(local.org, "_assembly_stats_", db, 
                                       ".txt")
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
                        message(
                            "The download session seems to have timed out at the FTP site '",
                            download_url, "'. This could be due to an overload of queries to the databases.",
                            " Please restart this function to continue the data retrieval process or wait ",
                            "for a while before restarting this function in case your IP address was logged due to an query overload on the server side."
                        )
                        return("Not available")
                    })
                }
                
                docFile(
                    file.name = 
                        paste0(local.org, "_assembly_stats_", db, ".txt"),
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
                        "The assembly statistics file of '",
                        organism,
                        "' has been downloaded to '",
                        path,
                        "' and has been named '",
                        paste0(local.org, "_assembly_stats_", db, ".txt"),
                        "'."
                    )
                )

                if (type == "download")
                    return(file.path(
                        path,
                        paste0(local.org, "_assembly_stats_", db, ".txt")
                    ))
                if (type == "import") {
                    assembly_stats_file <- read_assemblystats(file.path(
                        path,
                        paste0(local.org, "_assembly_stats_", db, ".txt")
                    ), type = "stats")
                    
                    assembly_stats_file <-
                        dplyr::bind_cols(tibble::tibble(species = species),
                                         assembly_stats_file)
                    return(assembly_stats_file)
                }
            } else {
                message(
                    "File: ",
                    download_url,
                    " could not be loaded properly... 
                     Something went wrong with the internet connection."
                )
        }
    }
}








