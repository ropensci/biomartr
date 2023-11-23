#' @title Helper function to retrieve species information from the ENSEMBL API
#' @description This function interfaces with the ENSEMBL API
#' (https://rest.ensembl.org/info/species?content-type=application/json)
#' and internally stores the output to use this information for subsequent
#' retrieval function calls.
#' @param update logical, default FALSE. If TRUE, force re-download of info.
#' @param division the ENSEMBL database (division) for which information shall
#' be retrieved (available options can be obtained with \code{\link{ensembl_divisions}}).
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # Look at available ENSEMBL division options
#' ensembl_divisions()
#' # Retrieve available information for EnsemblVertebrates
#' example <- get.ensembl.info(division = "EnsemblVertebrates")
#' example
#' # Update information file stored in the tempdir() folder.
#' example_update <- get.ensembl.info(division = "EnsemblVertebrates", update = TRUE)
#' example_update
#' }
#' @seealso \code{\link{ensembl_divisions}}, \code{\link{getKingdomAssemblySummary}}, \code{\link{getENSEMBLInfo}}
#' @export
get.ensembl.info <- function(update = FALSE, division) {
  tmp_file <- file.path(cachedir(), paste0(division, "_info.tsv"))
  if (file.exists(tmp_file) &&
        !update) {
        suppressWarnings(
            ensembl.info <-
                readr::read_tsv(
                    tmp_file,
                    col_names = TRUE,
                    col_types = readr::cols(
                        division = readr::col_character(),
                        taxon_id = readr::col_integer(),
                        name = readr::col_character(),
                        release = readr::col_integer(),
                        display_name = readr::col_character(),
                        accession = readr::col_character(),
                        common_name = readr::col_character(),
                        assembly = readr::col_character()
                    )
                )
        )

    } else {
        rest_url <- ensembl_rest_url_species_division(division)
        rest_api_status <- curl::curl_fetch_memory(rest_url)
        if (rest_api_status$status_code != 200) {
            message(
                "The API '", ensembl_rest_url(), "' does not seem to
                respond or work properly. Is the homepage '", ensembl_rest_url(), "' currently available?",
                " Could it be that there is a firewall issue on your side? Please re-run the function and check if it works now."
            )
        }

            ensembl.info <-
                tibble::as_tibble(
                    jsonlite::fromJSON(
                        rest_url
                    )$species
                )

        aliases <- groups <- NULL
        ensembl.info <-
            dplyr::select(ensembl.info, -aliases, -groups)

        readr::write_tsv(ensembl.info, tmp_file)
    }

    return(ensembl.info)
}


uniprot_stats_url <- function(){
  return("https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes/STATS")
}


ensembl_rest_url <- function() {
  "https://rest.ensembl.org"
}

#' @title List all available ENSEMBL divisions
#' @description
#' Retrieve a list of available databases on ENSEMBL for which \code{get.ensembl.info} can be retrieved.
#' @author Hajk-Georg Drost
#' @examples
#' ensembl_divisions()
#' @export
ensembl_divisions <- function() {
  c("EnsemblVertebrates", "EnsemblPlants", "EnsemblFungi", "EnsemblMetazoa",
    "EnsemblBacteria", "EnsemblProtists")
}

ensembl_divisions_short <- function(ensembl_as_empty = TRUE, bacteria = TRUE) {
  div  <-
    c(EnsemblVertebrates = "", EnsemblPlants = "plants", EnsemblFungi = "fungi",
    EnsemblBacteria = "bacteria", EnsemblMetazoa = "metazoa", EnsemblProtists = "protists")
  if (!ensembl_as_empty) div[1] <- "ensembl"
  if (!bacteria) div <- div[!(div == "bacteria")]
  return(div)
}

ensembl_rest_url_species <- function() {
  file.path(ensembl_rest_url(), "info/species")
}


#' Get supported species from given division
#'
#' @param division "EnsemblVertebrates", alternatives:
#'  "EnsemblPlants", "EnsemblFungi", "EnsemblMetazoa", "EnsemblBacteria"
#' @noRd
ensembl_rest_url_species_division <- function(division = "EnsemblVertebrates") {
  stopifnot(division %in% ensembl_divisions())
  suffix_url <- paste0("?division=", division,
                       "&content-type=application/json")
  file.path(ensembl_rest_url_species(), suffix_url)
}

ensembl_current_release <- function(division = "EnsemblVertebrates") {
  if (division == "EnsemblVertebrates") {
    jsonlite::fromJSON(
      file.path(ensembl_rest_url(), "info/data/?content-type=application/json")
    )$releases
  } else {
    jsonlite::fromJSON(
      file.path(ensembl_rest_url(),
                "info/eg_version?content-type=application/json"))$version
  }
}

ensembl_all_releases <- function(division = "EnsemblVertebrates") {
  seq_len(as.integer(ensembl_current_release(division)))
}

ensembl_rest_url_assembly <- function(organism) {
  file.path(ensembl_rest_url(),
            paste0("info/assembly/", organism,
                   "?content-type=application/json"
  ))
}

ensembl_ftp_server_url <- function(division = "EnsemblVertebrates") {
  if (division == "EnsemblVertebrates") {
    "https://ftp.ensembl.org"
  } else {
    "https://ftp.ensemblgenomes.ebi.ac.uk" # old: http://ftp.ensemblgenomes.org
  }
}

ensembl_ftp_server_url_release <- function(division, release = NULL) {
  if (division == "EnsemblVertebrates") {
    if (is.null(release)) {
      "pub/current_"
    } else paste0("pub/release-", release ,"/")

  } else {ensembl_divisions
    short_name <- ensembl_divisions_short()[division]
    if (is.null(release)) {
      paste0("pub/current/", short_name, "/")
    } else paste0("pub/release-", release ,"/", short_name, "/")
  }
}

ensembl_ftp_server_url_format <- function(division, release = NULL, format) {
  release_stem <- ensembl_ftp_server_url_release(division, release)
  paste0(release_stem, format, "/")
}

ensembl_ftp_server_url_format_full <- function(division, release = NULL, format) {
  paste0(ensembl_ftp_server_url(division), "/",
         ensembl_ftp_server_url_format(division, release, format))
}

ensembl_ftp_server_url_release_style_fasta <- function(division, release = NULL) {
  ensembl_ftp_server_url_format(division, release, "fasta")
}

ensembl_ftp_server_url_release_style_gtf <- function(division, release = NULL, format = "gtf") {
  ensembl_ftp_server_url_format(division, release, format)
}


ensembl_ftp_server_url_fasta <- function(division, release = NULL) {
  paste0(ensembl_ftp_server_url(division), "/",
         ensembl_ftp_server_url_release_style_fasta(division, release))
}

ensembl_ftp_server_url_gtf <- function(division = "EnsemblVertebrates",
                                       release = NULL, format = "gtf") {
  paste0(ensembl_ftp_server_url(division), "/",
         ensembl_ftp_server_url_release_style_gtf(division, release, format))
}

