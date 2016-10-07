#' @title List All Available Genomes
#' @description This function retrieves the names of all genomes available on the NCBI ftp:// server and stores
#' the results in a file named 'overview.txt' inside the directory '_ncbi_downloads' that
#' is built inside the workspace. 
#' @param kingdom a character string specifying a potential filter of available genomes: "all","Archaea", "Bacteria", "Eukaryota", "Viroids", "Viruses".
#' @param details a boolean value specifying whether only the scientific names of stored genomes shall be returned
#' (details = FALSE) or all information such as "organism_name","kingdoms", "group","subgroup","file_size_MB",
#' "chrs","organelles","plasmids", and "bio_projects".
#' @param update a logical value specifying whether or not the available organism table shall be updated from the NCBI server.
#' Default is \code{update} = \code{FALSE}.
#' @param db a character string specifying the database for which genome availability shall be checked, 
#' e.g. \code{db} =  \code{"refseq"} or \code{db} =  \code{"all"}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/} and creates a directory '_ncbi_downloads' in the \code{temdir()}
#' folder to store the overview.txt file for future processing. In case the overview.txt file already exists within the
#' '_ncbi_downloads' folder and is accessible within the workspace, no download process will be performed again.
#' @return A data.frame storing either the organism names (details = FALSE)
#' or all information present on the NCBI database (details = TRUE).
#' @note
#' 
#' Please note that the ftp:// connection relies on the NCBI server and cannot be
#' accurately accessed via a proxy. 
#' 
#' @examples \dontrun{
#' 
#' # the simplest way to retrieve all names of genomes stored within NCBI databases
#' head(listGenomes() , 5)
#' 
#' # show all details
#' head(listGenomes(details = TRUE) , 5)
#' 
#' # show all details only for Bacteria
#' head(listGenomes(kingdom = "Bacteria", details = TRUE) , 5)
#' 
#' # in case you are interested in the number of genomes available for each kingdom, run:
#' 
#' ncbi_genomes <- listGenomes(details = TRUE)
#' table(ncbi_genomes[ , "kingdoms"])
#' 
#' # analogous, if you are interested in the number of genomes available for each group, run:
#' ncbi_genomes <- listGenomes(details = TRUE)
#' table(ncbi_genomes[ , "group"])
#' 
#' # for subgroup
#' table(ncbi_genomes[ , "subgroup"])
#' 
#' # you can also limit your search to the refseq database
#' head(listGenomes(db = "refseq") , 20)
#' 
#' head(listGenomes(details=TRUE, db = "refseq") , 5)
#' 
#' head(listGenomes(kingdom = "Eukaryota", details = TRUE,db = "refseq") , 5)
#' 
#' # order by file size
#' library(dplyr)
#' head(arrange(ncbi_genomes, desc(file_size_MB)) , 5)
#' 
#' 
#' # you can also update the organism table using the 'update' argument
#' head(listGenomes(details = TRUE,update = TRUE) , 5)
#' 
#' }
#' 
#' @references \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt}
#' @export
listGenomes <- function(kingdom = "all", details = FALSE, update = FALSE, db = "all"){
        
    kingdoms <- organism_name <- NULL
    
    if (!is.element(kingdom,
                    c("all", "Archaea", "Bacteria", "Eukaryota", "Viroids", "Viruses")))
        stop("Please use a valid kingdom.")
    
    if (!is.element(db, c("refseq", "all")))
        stop("Please specify a database that is supported by this function.")
    
    
    if (!file.exists(file.path(tempdir(), "_ncbi_downloads"))) {
        dir.create(file.path(tempdir(), "_ncbi_downloads"))
    }

    if (!file.exists(file.path(tempdir(), "_ncbi_downloads", "overview.txt"))) {
        utils::download.file(
            "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt",
            file.path(tempdir(), "_ncbi_downloads", "overview.txt"),
            quiet = TRUE
        )
        
        # NCBI limits requests to three per second
        Sys.sleep(0.33)
    }
    
    if (update) {
        utils::download.file(
            "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt",
            file.path(tempdir(), "_ncbi_downloads", "overview.txt"),
            quiet = TRUE
        )
        
        # NCBI limits requests to three per second
        Sys.sleep(0.33)
    }
    
    col_classes <- vector(mode = "character", length = 9)
    col_classes <- c(rep("character", 4), rep("numeric", 5))
    ncbi_overview <-
        utils::read.csv(
            file.path(tempdir(), "_ncbi_downloads", "overview.txt"),
            sep = "\t",
            header = TRUE,
            colClasses = col_classes,
            na.strings = "-"
        )
    
    names(ncbi_overview) <- c(
        "organism_name",
        "kingdoms",
        "group",
        "subgroup",
        "file_size_MB",
        "chrs",
        "organelles",
        "plasmids",
        "bio_projects"
    )
    
    if (db == "refseq") {
        if (!file.exists(file.path(tempdir(), "_ncbi_downloads", "refseqOrgs.txt"))) {
            file.create(file.path(tempdir(), "_ncbi_downloads", "refseqOrgs.txt"))
            
            utils::write.table(
                x         = refseqOrganisms(),
                file      = file.path(tempdir(), "_ncbi_downloads", "refseqOrgs.txt"),
                sep       = "\n",
                quote     = FALSE,
                col.names = FALSE,
                row.names = FALSE
            )
        }
        
        refseqOrgs <-
            utils::read.table(
                file.path(tempdir(), "_ncbi_downloads", "refseqOrgs.txt"),
                header           = FALSE,
                sep              = "\n",
                colClasses       = "character",
                stringsAsFactors = FALSE
            )
        
        ncbi_overview <-
            ncbi_overview[stats::na.omit(match(refseqOrgs[, 1]
                                               , ncbi_overview[, "organism_name"])),]
    }
    
    if (kingdom == "all") {
        if (details == TRUE) {
            return(ncbi_overview)
        }
        
        if (details == FALSE) {
            return(ncbi_overview[, "organism_name"])
        }
    }
    
    if (kingdom != "all") {
        if (details == TRUE) {
            return(dplyr::filter(ncbi_overview, kingdoms == kingdom))
        }
        
        if (details == FALSE) {
            return(dplyr::select(
                dplyr::filter(ncbi_overview, kingdoms == kingdom),
                organism_name
            ))
        }
    }
}

