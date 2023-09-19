#' @title Helper function for retrieving gff files from ENSEMBL
#' @description This function downloads gff
#' files of query organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type specification type.
#' @param id.type id type.
#' @param release the ENSEMBL release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBL.Annotation <-
    function(organism,
             type = "dna",
             id.type = "toplevel",
             release = NULL,
             path) {

        if (!is.element(type, c("dna", "cds", "pep")))
            stop("Please a 'type' argument supported by this function:
                 'dna', 'cds', 'pep'.")

      ensembl_summary <- ensembl_assembly_hits(organism)
      if (isFALSE(ensembl_summary)) return(FALSE)

      # Check if assembly can be reached
      new.organism <- ensembl_proper_organism_name(ensembl_summary)
      rest_url <- ensembl_rest_url_assembly(new.organism)
      rest_api_status <- test_url_status(url = rest_url, organism = organism)
      if (is.logical(rest_api_status)) {
          return(FALSE)
      } else {
            species_api_url <- "http://rest.ensembl.org/info/species?content-type=application/json"
            if (curl::curl_fetch_memory(species_api_url)$status_code != 200) {
                message("The API connection to 'http://rest.ensembl.org/info/species?content-type=application/json' was not available.",
                        " Please make sure that you have a stable internet connection or are not blocked by any firewall.")
                return(FALSE)
            }

            ensembl.available.organisms <-
                jsonlite::fromJSON(species_api_url)


            aliases <- groups <- NULL
            # transform list object returned by 'fromJSON' to tibble
            ensembl.available.organisms <-
                tibble::as_tibble(dplyr::select(
                    ensembl.available.organisms$species,
                    -aliases,
                    -groups
                ))
        }

        release_api <- ensembl_current_release()
        if (!is.null(release)){
                if (!is.element(release, seq_len(as.integer(release_api))))
                        stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
        }

        # construct retrieval query
        core_path <- ensembl_ftp_server_url_gtf(format = "gff3")
        json.qry.info <- rest_api_status
        # construct retrieval query
        ensembl.qry <-
            paste0(
                    core_path,
                stringr::str_to_lower(new.organism),
                "/",
                paste0(
                    stringr::str_to_title(new.organism, locale = "en"),
                    ".",
                    json.qry.info$default_coord_system_version,
                    ".",
                    ifelse(is.null(release), ensembl.available.organisms$release[1], release),
                    ".gff3.gz"
                )
            )

        local_file <- file.path(
          path,
          paste0(
            stringr::str_to_title(new.organism, locale = "en"),
            ".",
            json.qry.info$default_coord_system_version,
            ".",
            ifelse(is.null(release), ensembl.available.organisms$release[1], release),
            "_ensembl",
            ".gff3.gz"
          )
        )

        if (file.exists(local_file)) {
            message("File ", local_file,
                    " exists already. Thus, download has been skipped.")
        } else {
                tryCatch({
                        custom_download(ensembl.qry, destfile = local_file,
                                        mode = "wb")
                }, error = function(e) {
                        message(
                                "Something went wrong when trying to retrieve file ",
                                ensembl.qry,
                                " from ENSEMBL. Could it be that the species ",
                                organism,
                                " does not have an entry for your specified release version?"
                        )
                })
        }
        return(c(local_file, ensembl.qry))
}
