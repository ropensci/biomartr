#' @title Retrieve UniProt Database Information File (STATS)
#' @description
#' The UniProt stores a \code{STATS} file to summarise all available information for their reference proteomes.
#' Users can now download this file and process it with \code{biomartr}.
#' @param update shall the internal \code{\link{cachedir}} file be deleted and the \code{STATS}
#' file freshly downloaded from the UniProt FTP servers? 
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # retrieve STATS file from UniProt
#' uniprot_info <- getUniProtSTATS(update = TRUE)
#' # look at results
#' uniprot_info
#' }
#' @export

getUniProtSTATS <- function(update = FALSE) {
  message("-> Start UniProt information retrieval from: https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes/STATS")
  tmp_file <- file.path(cachedir(),  "UniProtSTATS.tsv")
  if (file.exists(tmp_file) &&
      !update) {
    message("-> It seems a local version of the file is already available and 'update = FALSE', so the local file is being imported.")
    suppressWarnings(
      uniprot.info <-
        readr::read_tsv(
          tmp_file,
          skip = 16, # the first 16 lines of the STATS file are only annotation and not part of the dataset 
          col_names = FALSE,
          col_types = readr::cols(
            "X1" = readr::col_character(),
            "X2" = readr::col_integer(),
            "X3" = readr::col_character(),
            "X4" = readr::col_integer(),
            "X5" = readr::col_character(),
            "X6" = readr::col_character(),
            "X7" = readr::col_character(),
            "X8" = readr::col_character()
          )
        )
    )
    names(uniprot.info) <-
      c(
        "proteome_id",
        "taxid",
        "n_entries_fasta_canonical",
        "n_entries_fasta_isoforms",
        "n_entries_fasta_gene2acc_mapping",
        "assemby_release",
        "source_of_proteome_import",
        "species_name"
      )
  } else {
    rest_url <- uniprot_stats_url()
    rest_api_status <- curl::curl_fetch_memory(rest_url)
    if (rest_api_status$status_code != 200) {
      message(
        "The API '", uniprot_stats_url(), "' does not seem to
                respond or work properly. Is the homepage '", uniprot_stats_url(), "' currently available?",
        " Could it be that there is a firewall issue on your side? Please re-run the function and check if it works now."
      )
    }
    # download STATS file
    custom_download(url = uniprot_stats_url(), destfile = tmp_file)
    
    suppressWarnings(
      uniprot.info <-
        readr::read_tsv(
          tmp_file,
          skip = 16, # the first 15 lines of the STATS file are only annotation and not part of the dataset 
          col_names = FALSE,
          col_types = readr::cols(
            "X1" = readr::col_character(),
            "X2" = readr::col_integer(),
            "X3" = readr::col_character(),
            "X4" = readr::col_integer(),
            "X5" = readr::col_character(),
            "X6" = readr::col_character(),
            "X7" = readr::col_character(),
            "X8" = readr::col_character()
          )
        )
    )
    names(uniprot.info) <-
      c(
        "proteome_id",
        "taxid",
        "n_entries_fasta_canonical",
        "n_entries_fasta_isoforms",
        "n_entries_fasta_gene2acc_mapping",
        "assemby_release",
        "source_of_proteome_import",
        "species_name"
      )
  }
  message("-> Import successful!")
  return (uniprot.info)
}



