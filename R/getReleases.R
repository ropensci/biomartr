#' @title Retrieve available database releases or versions 
#' of ENSEMBL
#' @description Retrieve available database releases or versions of 
#' ENSEMBL.
#' @param db a character string specifying the database from which 
#' available resease versions shall be retrieved:
#' \itemize{
#' \item \code{db = "ensembl"}
#' }
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{ 
#' # retrieve available resease versions of ENSEMBL
#' getReleases("ensembl")
#' }
#' @export
getReleases <- function(db = "ensembl") {
    
    if (!is.element(db, c("ensembl")))
        stop(
            "Please select one of the available databases: 'ensembl'.",
            call. = FALSE
        )
    
    if (db == "ensembl") {
      
        tryCatch({  
        current_release <-
            jsonlite::fromJSON(
"http://rest.ensembl.org/info/data/?content-type=application/json")$releases
        }, error = function(e)
            message(
                "The API 'http://rest.ensembl.org' does not seem 
                to be reachable. Could you please check whether you are connected to the internet? 
                Is it possible to access the homepage 'http://rest.ensembl.org' 
                via your browser?"
            ))
        
        message("The current ENSEMBL release is release-", current_release, ".")
        return(paste0("release-", seq_len(current_release)))
    }

    if (db == "ensemblgenomes") {
        
        tryCatch({  
            current_release <-
                jsonlite::fromJSON(
"http://rest.ensembl.org/info/data/?content-type=application/json")$releases
        }, error = function(e)
          message(
            "The API 'http://rest.ensembl.org' does not seem 
                to be reachable. Could you please check whether you are connected to the internet? 
                Is it possible to access the homepage 'http://rest.ensembl.org' 
                via your browser?"
          ))
        
        message("The current ENSEMBL release is release-",
                current_release, ".")
        return(paste0("release-", seq_len(current_release)))
    }
}
