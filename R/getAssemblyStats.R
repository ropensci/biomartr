#' @title Genome Assembly Stats Retrieval
#' @description  Main genome assembly stats retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest the corresponding  genome assembly stats file storing the assembly statistics of the organism of interest
#' can be downloaded and stored locally. Genome assembly stats files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{db = "refseq"} or \code{db = "genbank"}.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param type shall only the file be retrieved (default) \code{type = "download"} or should the corresponding file be downloaded and subsequently be imported \code{type = "import"}.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' file shall be stored. Default is \code{path} = \code{file.path("_ncbi_downloads","genomeassembly_stats")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'  
#' to retrieve available scientific names of organisms and creates a directory '_ncbi_downloads/genomeassembly_stats' to store
#' the Genome Assembly Stats of interest as text file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomeassembly_stats' folder and is accessible within the workspace,
#' no download process will be performed.
#' 
#' An example genome assembly stats file can be found here: ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/001/405/GCF_000001405.36_GRCh38.p10/GCF_000001405.36_GRCh38.p10_assembly_stats.txt.
#' 
#' @return File path to downloaded genome assembly stats file.
#' @examples \dontrun{
#' # download the genome assembly stats file of Homo sapiens from NCBI RefSeq
#' # and store the corresponding genome file in '_ncbi_downloads/genomeassembly_stats'
#' file_path <- getAssemblyStats( db = "refseq", 
#'                  organism = "Homo sapiens", 
#'                  path = file.path("_ncbi_downloads","genomeassembly_stats"))
#' # import the raw file as it is downloaded
#' Hsapiens.stats <- read_assemblystats(file_path, type = "raw")
#' 
#' # download the genome assembly stats file of Homo sapiens from NCBI RefSeq 
#' # and import overall statistics of the genome assembly
#' Hsapiens.stats.import <- getAssemblyStats( db = "refseq", 
#'                  organism = "Homo sapiens",
#'                  type = "import", 
#'                  path = file.path("_ncbi_downloads","genomeassembly_stats"))
#' }
#' 
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}}, \code{\link{getGFF}}, \code{\link{meta.retrieval}}, \code{\link{read_assemblystats}}
#' @export

getAssemblyStats <-
    function(db = "refseq",
             organism,
             type = "download",
             path = file.path("_ncbi_downloads", "genomeassembly_stats")) {
        if (!is.element(db, c("refseq", "genbank")))
            stop("Please select one of the available data bases: 'refseq' and 'genbank'.",
                 call. = FALSE)
        
        
        # get Kingdom Assembly Summary file
        AssemblyFilesAllKingdoms <-
            getKingdomAssemblySummary(db = db)
        
        if (!is.element(type, c("download", "import")))
            stop(
                "Please choose either type = 'download' (if you would like to download the genome assembly stats file) or type = 'import' (if you would like to download and import the genome assembly stats file).",
                call. = FALSE
            )
        
        species <- organism
        # test wheter or not genome is available
        is.genome.available(organism = organism, db = db)
        
        if (!file.exists(path)) {
            dir.create(path, recursive = TRUE)
        }
        
        organism_name <-
            refseq_category <- version_status <- NULL
        
        organism <- stringr::str_replace_all(organism, "\\(", "")
        organism <- stringr::str_replace_all(organism, "\\)", "")
        
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
            cat(
                paste0(
                    "----------> No genome assembly stats file for a reference genome or representative genome was found for '",
                    organism,
                    "'. Thus, download for this species has been omitted."
                )
            )
            cat("\n")
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
            
            # download_url <- paste0(query$ftp_path,query$`# assembly_accession`,"_",query$asm_name,"_genomic.fna.gz")
            
            local.org <-
                stringr::str_replace_all(organism, "-", "_")
            local.org <-
                stringr::str_replace_all(organism, "\\/", "_")
            
            if (nrow(FoundOrganism) == 1) {
                tryCatch({
                    utils::capture.output(
                        downloader::download(
                            download_url,
                            destfile = file.path(
                                path,
                                paste0(local.org, "_assembly_stats_", db, ".txt")
                            ),
                            mode = "wb"
                        )
                    )
                }, error = function(e)
                    stop(
                        "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' cannot be reached. Are you connected to the internet? Is the the FTP site '",
                        download_url,
                        "' currently available?",
                        call. = FALSE
                    ))
                
                docFile(
                    file.name = paste0(local.org, "_assembly_stats_", db, ".txt"),
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
                    
                    assembly_stats_file <- dplyr::bind_cols(tibble::tibble(species = species), assembly_stats_file)
                    return(assembly_stats_file)
                }
            } else {
                stop(
                    "File: ",
                    download_url,
                    " could not be loaded properly... Are you connected to the internet?",
                    call. = FALSE
                )
            }
        }
    }








