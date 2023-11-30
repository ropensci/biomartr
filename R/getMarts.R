#' @title Retrieve information about available Ensembl Biomart databases
#' @description This funcion queries the Ensembl Biomart API and returns a table
#' storing information about all available Ensembl Biomart databases.
#' @param update logical, default FALSE. If FALSE, use cached file if it exists.
#' Set to TRUE to force new update
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # get a table of all available databases from Ensembl Biomart
#' getMarts()
#'  }
#' @family biomaRt
#' @export
getMarts <- function(update = FALSE) {
  file <- file.path(cachedir(), "available_biomarts_dbs.rds")
  if (!update && file.exists(file)) {
    message("-- Loading cached biomart database overview object")
    return(readRDS(file))
  }
  submarts <- ensembl_divisions_short(FALSE, bacteria = FALSE)
  submarts.df <- vector("list", length(submarts))

  # Only fail if primary mart is down, else notify which are down
  for (i in seq_along(submarts)) {
    tryCatch({
      submarts.df[i] <- list(getSubMarts(submarts[i]))
      }, error = function(e) {
      message(
          "It seems like the BioMart server could not be reached for mart: ", submarts[i],
          ". This is either due to a server maintainence downtime or may be due to an instable internet connection on your side."
      )
      if (i == 1) stop()
      })

  }
  marts <- dplyr::bind_rows(submarts.df)
  saveRDS(marts, file)

  return(marts)
}




