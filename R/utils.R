
#' @title Check Genome Availability
#' @description This function checks the availability of a given genome on the NBCI servers specified
#' as scientific name.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis thaliana'.
#' @param details a logical value specifying whether or not details on genome size, kingdom, etc. shall be printed to the
#' console intead of a boolean value.
#' @param database a character string specifying the database for which genome availability shall be checked,
#' e.g. \code{database} =  \code{"refseq"} or \code{database} =  \code{"all"}.
#' @details
#'
#' Internally this function calls the \code{\link{listGenomes}} function to detect all available genomes
#' and checks whether or not the specified organism is available for download.
#'
#' @return a logical value specifing whether or not the genome of the input organism
#' is available. In case \code{details} = \code{TRUE} only a character string specifying the
#' genome details is being returned.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#'
#' # checking whether the Arabidopsis thaliana genome is stored on NCBI
#' is.genome.available(organism = "Arabidopsis thaliana")
#'
#' # and printing details
#' is.genome.available(organism = "Arabidopsis thaliana", details = TRUE)
#'
#' }
#' @references
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt}
#' @export

is.genome.available <- function(organism, details = FALSE, database = "refseq"){


        available_genome <- listGenomes("all",TRUE,database = database)

        is_available <- any(stringr::str_detect(available_genome[ , "organism_name"],organism))


        if(is_available){

                organism_index <- which(stringr::str_detect(available_genome[ , "organism_name"],organism))

                if(details){

                        return(available_genome[organism_index, ])

                } else {

                        return(TRUE)
                }

        } else {

                return(FALSE)

        }

}



docFile <- function(file.name, organism, url, database, path){

        cwd <- getwd()

        setwd(path)

        sink(paste0("doc_",organism,"_db_",database,".txt"))

        cat(paste0("File Name: ", file.name))
        cat("\n")
        cat(paste0("Organism Name: ", organism))
        cat("\n")
        cat(paste0("Database: ", database))
        cat("\n")
        cat(paste0("URL: ", url))
        cat("\n")
        cat(paste0("Date: ", date()))

        sink()

        setwd(cwd)

}



setTMPFile <- function(file.name){

        tempfile(pattern = file.name, tmpdir = tempdir(), fileext = "")
}


getTMPFile <- function(file.name){

        file.path(tempdir(),file.name)

}



test <- function(x){ print(paste0("Test ",x," passed.","\n"))}
