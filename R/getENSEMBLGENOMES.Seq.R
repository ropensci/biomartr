#' @title Helper function for retrieving biological sequence files from
#' ENSEMBLGENOMES
#' @description This function downloads gff files of query organisms from
#' ENSEMBLGENOMES
#' @param organism scientific name of the organism of interest.
#' @param release the ENSEMBLGENOMES release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param type biological sequence type.
#' @param id.type id type.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBLGENOMES.Seq <-
    function(organism,
             release = NULL,
             type = "dna",
             id.type = "toplevel",
             path) {

        if (!is.element(type, c("dna", "cds", "pep", "ncrna")))
            stop("Please a 'type' argument supported by this function:
                 'dna', 'cds', 'pep', 'ncrna'.")

        name <- NULL
        # test if REST API is responding
        is.ensemblgenomes.alive()

        if (is.taxid(organism))
            stop("Unfortunately, taxid retrieval is not yet implemented for ENSEMBLGENOMES...", call. = FALSE)

        ensembl_summary <- ensembl_assembly_hits(organism)
        if (isFALSE(ensembl_summary)) return(FALSE)
        # message("Several entries were found for '", organism, "'.")
        # #    "... The first entry '", ensembl_summary$name[1],"' with accession id '",ensembl_summary$accession[1],"' was selected for download.")
        # message("In case you wish to retrieve another genome version please consult is.genome.available(organism = '", organism,"', details = TRUE, db = 'ensemblgenomes') and specify another accession id as organism argument.")
        # message("\n")
        # select only first entry

        new.organism <- ensembl_proper_organism_name(ensembl_summary)
        rest_url <- ensembl_rest_url_assembly(new.organism)
        rest_api_status <- test_url_status(url = rest_url, organism = organism)
        if (isFALSE(rest_api_status)) return(FALSE)

        if (ensembl_summary$division[1] == "EnsemblBacteria") {

          bacteria_collection <- get_bacteria_collection_id(ensembl_summary)
          if (isFALSE(bacteria_collection)) return(FALSE)

            release_api <- jsonlite::fromJSON(
                    "http://rest.ensembl.org/info/eg_version?content-type=application/json"
            )

            if (!is.null(release)){
                    if (!is.element(release, seq_len(as.integer(release_api))))
                            stop("Please provide a release number that is supported by ENSEMBLGENOMES.", call. = FALSE)
            }

            # parse for wrong name conventions and fix them...
            organism <-
              stringr::str_replace_all(organism, " sp ", " sp. ")
            organism <-
              stringr::str_replace_all(organism, " pv ", " pv. ")
            organism <-
              stringr::str_replace_all(organism, " str ", " str. ")
            organism <-
              stringr::str_replace_all(organism, " subsp ", " subsp. ")
            organism <-
              stringr::str_replace_all(organism, "\\(", "")
            organism <-
              stringr::str_replace_all(organism, "\\)", "")

            # construct retrieval query
            if (is.null(release))
                    core_path <- "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/fasta/"

            if (!is.null(release))
                    core_path <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/bacteria/fasta/")

            ensembl.qry <-
               paste0(core_path,
                      bacteria_collection,
                      stringr::str_to_lower(new.organism),
                      "/",
                      type,
                      "/",
                      paste0(
                          new.organism,
                          ".",
                          rest_api_status$default_coord_system_version,
                          ".",
                          type,
                          ifelse(id.type == "none", "", "."),
                          ifelse(id.type == "none", "", id.type),
                          ".fa.gz"
                      )
                )

        } else {

                release_api <- jsonlite::fromJSON(
                        "http://rest.ensembl.org/info/eg_version?content-type=application/json"
                )

                if (!is.null(release)){
                        if (!is.element(release, seq_len(as.integer(release_api))))
                                stop("Please provide a release number that is supported by ENSEMBLGENOMES.", call. = FALSE)
                }

                # construct retrieval query
                if (is.null(release))
                        core_path <- "ftp://ftp.ensemblgenomes.org/pub/current/"

                if (!is.null(release))
                        core_path <- paste0("ftp://ftp.ensemblgenomes.org/pub/release-", release ,"/")

            # construct retrieval query
            ensembl.qry <-
                paste0( core_path,
                    stringr::str_to_lower(
                        stringr::str_replace(get.org.info$division[1],
                                             "Ensembl", "")
                    ),
                    "/fasta/",
                    stringr::str_to_lower(new.organism),
                    "/",
                    type,
                    "/",
                    paste0(
                        new.organism,
                        ".",
                        rest_api_status$default_coord_system_version,
                        ".",
                        type,
                        ifelse(id.type == "none", "", "."),
                        ifelse(id.type == "none", "", id.type),
                        ".fa.gz"
                    )
                )
        }

        if (file.exists(file.path(
            path,
            paste0(
                new.organism,
                ".",
                rest_api_status$default_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none", "", "."),
                ifelse(id.type == "none", "", id.type),
                ".fa.gz"
            )
        ))) {
            message(
                "File ",
                file.path(
                    path,
                    paste0(
                        new.organism,
                        ".",
                        rest_api_status$default_coord_system_version,
                        ".",
                        type,
                        ifelse(id.type == "none", "", "."),
                        ifelse(id.type == "none", "", id.type),
                        ".fa.gz"
                    )
                ),
                " exists already. Thus, download has been skipped."
            )
        } else {
                custom_download(url = ensembl.qry,
                                destfile = file.path(
                                    path,
                                    paste0(
                                        new.organism,
                                        ".",
                                        rest_api_status$default_coord_system_version,
                                        ".",
                                        type,
                                        ifelse(id.type == "none", "", "."),
                                        ifelse(id.type == "none", "", id.type),
                                        ".fa.gz"
                                    )
                                ))
        }

        return(c(file.path(
            path,
            paste0(
                new.organism,
                ".",
                rest_api_status$default_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none", "", "."),
                ifelse(id.type == "none", "", id.type),
                ".fa.gz"
            )
        ), ensembl.qry))

}
