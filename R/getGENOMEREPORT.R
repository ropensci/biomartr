#' @title Retrieve NCBI GENOME_REPORTS file
#' @description Retrieves NCBI GENOME_REPORTS file from 
#' ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' report <- getGENOMEREPORT()
#' report
#' }
#' @export

getGENOMEREPORT <- function() {
  withr::local_options(timeout = max(30000, getOption("timeout")))
  
    if (!file.exists(file.path(tempdir(), "_ncbi_downloads"))) {
        dir.create(file.path(tempdir(), "_ncbi_downloads"))
    }
    
    if (!file.exists(file.path(tempdir(), "_ncbi_downloads", "overview.txt"))) {
        tryCatch({
            downloader::download(
               "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt",
                file.path(tempdir(), "_ncbi_downloads", "overview.txt"),
                mode = "wb"
            )
        }, error = function(e)
            message(
                "Unfortunately, the FTP site 
                'ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt'
                does not seem to be reachable. Is your current internet connection stable? Can you reach the
                FTP site 'ftp://ftp.ncbi.nlm.nih.gov'?"
            ))
        
        # NCBI limits requests to three per second
    }
    
    suppressWarnings(
        ncbi_overview <-
            readr::read_tsv(
                file.path(tempdir(), "_ncbi_downloads", "overview.txt"),
                comment = "#",
                col_names = c(
                    "organism_name",
                    "kingdoms",
                    "group",
                    "subgroup",
                    "file_size_MB",
                    "chrs",
                    "organelles",
                    "plasmids",
                    "bio_projects"
                ),
                col_types = readr::cols(
                    organism_name = readr::col_character(),
                    kingdoms = readr::col_character(),
                    group = readr::col_character(),
                    subgroup = readr::col_character(),
                    file_size_MB = readr::col_double(),
                    chrs = readr::col_integer(),
                    organelles = readr::col_integer(),
                    plasmids = readr::col_integer(),
                    bio_projects = readr::col_integer()
                )
            )
    )
    
    return(ncbi_overview)
}

