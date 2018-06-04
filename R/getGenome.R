#' @title Genome Retrieval
#' @description  Main genome retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest the 
#' corresponding fasta-file storing the genome of the organism of interest
#' can be downloaded and stored locally. Genome files can be retrieved from 
#' several databases.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' \item \code{db = "ensemblgenomes"}
#' }
#' @param organism there are three options to characterize an organism: 
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome. 
#' @param path a character string specifying the location (a folder) in which 
#' the corresponding genome shall be stored. Default is 
#' \code{path} = \code{file.path("_ncbi_downloads","genomes")}.
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
#' @return File path to downloaded genome.
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
#' 
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}}, 
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}}, 
#' \code{\link{read_genome}}
#' @export

getGenome <-
    function(db = "refseq",
             organism,
             reference = TRUE,
             path = file.path("_ncbi_downloads", "genomes")) {
        
       if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
            stop(
                "Please select one of the available data bases: 'refseq',
                'genbank', 'ensembl', or 'ensemblgenomes'.",
                call. = FALSE
            )
        
        if (!is.logical(reference))
            stop("Please specify 'reference' as either TRUE or FALSE.", call. = FALSE)
                
                
        if (db == "ensemblgenomes") {
            organism_name <- is.genome.available(db = db, organism = organism, details = TRUE)$display_name
            message("Starting genome retrieval of '", organism_name, "' from ", db, " ...")
            message("\n")
        } else {
            message("Starting genome retrieval of '", organism, "' from ", db, " ...")
            message("\n")
        }
        
        if (is.element(db, c("refseq", "genbank"))) {
            # get Kingdom Assembly Summary file
            AssemblyFilesAllKingdoms <-
                getKingdomAssemblySummary(db = db)
            
            # test wheter or not genome is available
            if (!suppressMessages(is.genome.available(organism = organism, db = db))) {
                    message(
                            "Unfortunately no genome file could be found for organism '",
                            organism, "'. Thus, the download of this organism has been omitted. Have you tried to specify 'reference = FALSE' ?"
                    )
                    return("Not available")
            }
            
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            organism_name <- taxid <-
                refseq_category <- version_status <- assembly_accession <- NULL
            
            organism <-
                stringr::str_replace_all(organism, "\\(", "")
            organism <-
                stringr::str_replace_all(organism, "\\)", "")
            
            if (reference) {
                if (!is.taxid(organism)) {
                    FoundOrganism <-
                        dplyr::filter(
                            AssemblyFilesAllKingdoms,
                            stringr::str_detect(organism_name, organism) | 
                                assembly_accession ==  organism,
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
                                assembly_accession == organism,
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
                        " Have you tried to specify getGenome(db = '",db,"', organism = '",organism,"' , reference = FALSE) ?",
                        " Alternatively, you can retrieve genome assemblies using the NCBI accession ID or NCBI Taxonomy ID.",
                        " See '?'is.genome.available' for examples."
                    )
                )
                 return("Not available")
            } else {
                if (nrow(FoundOrganism) > 1) {
                    warning(
                        "More than one entry has been found for '",
                        organism, "'. Only the first entry '", FoundOrganism$organism_name[1], "' has been used for subsequent genome retrieval.",
                        " If you wish to download a different version, please use the NCBI accession ID when specifying the 'organism' argument.",
                        " See ?is.genome.available for examples.", call. = FALSE
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
                               "_genomic.fna.gz"
                           ))
                
                
                
                # download_url <- paste0(query$ftp_path,query$`
                # assembly_accession`,"_",query$asm_name,"_genomic.fna.gz")
                
                local.org <-
                    stringr::str_replace_all(organism, "-", "_")
                local.org <-
                    stringr::str_replace_all(organism, "\\/", "_")
                
                if (nrow(FoundOrganism) == 1) {
                    if (file.exists(file.path(
                        path,
                        paste0(local.org, "_genomic_", db, ".fna.gz")
                    ))) {
                        message(
                            "File ",
                            file.path(
                                path,
                                paste0(local.org, "_genomic_", db, ".fna.gz")
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
                                               "_genomic_", db, ".fna.gz")
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
                            
                            message("Genome download is completed!")
                            
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
                                           "_genomic.fna.gz"
                                       )))$md5
                            
                            message("Checking md5 hash of file: ", 
                                    md5_file_path , " ...")
                            if (!(tools::md5sum(file.path(
                                path,
                                paste0(local.org, "_genomic_", db, ".fna.gz")
                            )) == md5_sum))
                                stop(
                                    paste0(
                                        "Please download the file '",
                                        md5_file_path,
            "' again. The md5 hash between the downloaded file and the file ", "stored at NCBI do not match.",
                                        collapse = ""
                                    )
                                )
                            unlink(md5_file_path)
            message("The md5 hash of file '", md5_file_path, "' matches!")
                            
                        }, error = function(e)
                            stop(
                                "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' cannot be reached. Are you connected to the internet? Is the the FTP site '",
                                download_url, "' currently available?",
                                call. = FALSE
                            ))
                    }
                    
                    docFile(
                        file.name = paste0(ifelse(is.taxid(organism), paste0("taxid_", local.org), local.org), "_genomic_", db, 
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
                            "The genome of '",
                            organism,
                            "' has been downloaded to '",
                            path,
                            "' and has been named '",
                            paste0(local.org, "_genomic_", db, ".fna.gz"),
                            "'."
                        )
                    )
                    
                    return(file.path(
                        path,
                        paste0(local.org, "_genomic_", db, ".fna.gz")
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
        
        if (db == "ensembl") {
            # create result folder
            if (!file.exists(path)) {
                dir.create(path, recursive = TRUE)
            }
            
            # download genome sequence from ENSEMBL
            genome.path <-
                getENSEMBL.Seq(organism, type = "dna", 
                               id.type = "toplevel", path = path)
            
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
                                    (accession == organism),
                                    !is.na(assembly)
                            )
                    }
                }
            
                new.organism <- ensembl_summary$name[1]
                new.organism <-
                    paste0(
                        stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
                        stringr::str_sub(new.organism, 2, nchar(new.organism))
                    )
                
                url_api <- paste0(
                    "http://rest.ensembl.org/info/assembly/",
                    new.organism,
                    "?content-type=application/json"
                )
                
                # choose only first entry if not specified otherwise
                if (length(url_api) > 1)
                    url_api <- url_api[1]
                
                if (curl::curl_fetch_memory(url_api)$status_code != 200) {
                    message("The API call '",url_api,"' did not work. This might be due to a non-existing organism that you specified or a corrupted internet or firewall connection.")
                    return("Not available")
                }
                
                # retrieve information from API
                json.qry.info <- jsonlite::fromJSON(url_api)
                
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
                cat(paste0("assembly_name: ", ifelse(!is.null(json.qry.info$assembly_name), json.qry.info$assembly_name, "none")))
                cat("\n")
                cat(paste0("assembly_date: ", ifelse(!is.null(json.qry.info$assembly_date), json.qry.info$assembly_date, "none")))
                cat("\n")
                cat(
                    paste0(
                        "genebuild_last_geneset_update: ",
                        ifelse(!is.null(json.qry.info$genebuild_last_geneset_update), json.qry.info$genebuild_last_geneset_update, "none")
                    )
                )
                cat("\n")
                cat(paste0(
                    "assembly_accession: ",
                    ifelse(!is.null(json.qry.info$assembly_accession), json.qry.info$assembly_accession, "none")
                ))
                cat("\n")
                cat(
                    paste0(
                        "genebuild_initial_release_date: ",
                        ifelse(!is.null(json.qry.info$genebuild_initial_release_date), json.qry.info$genebuild_initial_release_date, "none")
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
                        "The genome of '",
                        organism,
                        "' has been downloaded to '",
                        path,
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
                getENSEMBLGENOMES.Seq(organism, type = "dna", 
                                      id.type = "toplevel", path = path)
            
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
                                    (accession == organism),
                                    !is.na(assembly)
                            )
                    }
                }
                
                new.organism <- stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))
                new.organism <-
                    paste0(
                        stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
                        stringr::str_sub(new.organism, 2, nchar(new.organism))
                    ) 
                
                url_api <- paste0(
                    "http://rest.ensemblgenomes.org/info/assembly/",
                    new.organism,
                    "?content-type=application/json"
                )
                
                # choose only first entry if not specified otherwise
                if (length(url_api) > 1)
                    url_api <- url_api[1]
                
                if (curl::curl_fetch_memory(url_api)$status_code != 200) {
                    message("The API call '",url_api,"' did not work. This might be due to a non-existing organism that you specified or a corrupted internet or firewall connection.")
                    return("Not available")
                }
                
                # retrieve information from API
                json.qry.info <- jsonlite::fromJSON(url_api)
                
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
                cat(paste0("assembly_name: ", ifelse(!is.null(json.qry.info$assembly_name), json.qry.info$assembly_name, "none")))
                cat("\n")
                cat(paste0("assembly_date: ", ifelse(!is.null(json.qry.info$assembly_date), json.qry.info$assembly_date, "none")))
                cat("\n")
                cat(
                    paste0(
                        "genebuild_last_geneset_update: ",
                        ifelse(!is.null(json.qry.info$genebuild_last_geneset_update), json.qry.info$genebuild_last_geneset_update, "none")
                    )
                )
                cat("\n")
                cat(paste0(
                    "assembly_accession: ",
                    ifelse(!is.null(json.qry.info$assembly_accession), json.qry.info$assembly_accession, "none")
                ))
                cat("\n")
                cat(
                    paste0(
                        "genebuild_initial_release_date: ",
                        ifelse(!is.null(json.qry.info$genebuild_initial_release_date), json.qry.info$genebuild_initial_release_date, "none")
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
                        "The genome of '",
                        organism,
                        "' has been downloaded to '",
                        path,
                        "' and has been named '",
                        basename(genome.path),
                        "'."
                    )
                )
                
                return(genome.path)
        }
    }
}





