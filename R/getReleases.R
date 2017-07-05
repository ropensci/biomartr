#' @title Retrieve available database releases or versions 
#' of ENSEMBL and ENSEMBLGENOMES
#' @description Retrieve available database releases or versions of 
#' ENSEMBL and ENSEMBLGENOMES.
#' @param db a character string specifying the database from which 
#' available resease versions shall be retrieved:
#' \itemize{
#' \item \code{db = "ensembl"}
#' \item \code{db = "ensemblgenomes"}
#' }
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' # retrieve available resease versions of ENSEMBL
#' getReleases("ensembl")
#' # retrieve available resease versions of ENSEMBLGENOMES
#' getReleases("ensemblgenomes")
#' }
#' @export
getReleases <- function(db = "refseq") {
    
    if (!is.element(db, c("ensembl", "ensemblgenomes")))
        stop(
            "Please select one of the available databases: 'ensembl'
            or 'ensemblgenomes'.",
            call. = FALSE
        )
    
    if (db == "ensembl") {
      
        tryCatch({  
        current_release <-
            jsonlite::fromJSON(
"http://rest.ensembl.org/info/data/?content-type=application/json")$releases
        }, error = function(e)
            stop(
                "The API 'http://rest.ensembl.org' does not seem 
                to work properly. Are you connected to the internet? 
                Is the homepage 'http://rest.ensembl.org' 
                currently available?",
                call. = FALSE
            ))
        
        message("The current ENSEMBL release is release-", current_release, ".")
        return(paste0("release-", seq_len(current_release)))
    }

    if (db == "ensemblgenomes") {
        
        tryCatch({  
            current_release <-
                jsonlite::fromJSON(
"http://rest.ensemblgenomes.org/info/data/?content-type=application/json")$releases
        }, error = function(e)
            stop(
                "The API 'http://rest.ensemblgenomes.org' does not seem 
                to work properly. Are you connected to the internet? 
                Is the homepage 'http://rest.ensemblgenomes.org' 
                currently available?",
                call. = FALSE
            ))
        
        message("The current ENSEMBLGENOMES release is release-",
                current_release, ".")
        return(paste0("release-", seq_len(current_release)))
    }
}
