#' @title Helper function for retrieving gff files from ENSEMBL
#' @description This function downloads gff
#' files of query organisms from ENSEMBL.
#' @inheritParams getENSEMBL.Seq
#' @import curl RCurl
#' @author Hajk-Georg Drost
#' @return character filepath to download file, returns FALSE if failed.
getENSEMBL.gtf <- function(organism, type = "dna", id.type = "toplevel",
                           path, release = NULL) {

    if (!is.element(type, c("dna", "cds", "pep")))
            stop("Please a 'type' argument supported by this function:
                 'dna', 'cds', 'pep'.")

    ensembl_summary <- ensembl_assembly_hits(organism)
    if (isFALSE(ensembl_summary)) return(FALSE)

    new.organism <- ensembl_proper_organism_name(ensembl_summary)
    rest_url <- ensembl_rest_url_assembly(new.organism)
    rest_api_status <- test_url_status(url = rest_url, organism = organism)
    if (isFALSE(rest_api_status)) return(FALSE)

    if (!is.null(release)) {
      release <- as.numeric(release)
      if (!is.element(release, ensembl_all_releases()))
        stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
    } else release <- ensembl_current_release(ensembl_summary$division[1])


    # Before 75, ensembl used .release extension on assembly
    all_possible_assemblies <- rest_api_status$coord_system_versions
    all_possible_assemblies <- paste0(all_possible_assemblies,
                                      ".", release)
    if (is.numeric(release)) {
      if (release <= 46) {
        message("ensembl release <= 46 is not supported")
        return(FALSE)
      }
    }

    # construct retrieval query
    core_path <- ensembl_ftp_server_url_gtf(ensembl_summary$division[1],
                                              release)
    rest_api_status$release_coord_system_version <- "not_found"
    for (assembly_option in all_possible_assemblies) {
      collection_for_bacteria_only <- get_bacteria_collection_id(ensembl_summary)
      if (isFALSE(collection_for_bacteria_only)) return(FALSE)
      ensembl.qry <-
        paste0(
          core_path,
          collection_for_bacteria_only,
          stringr::str_to_lower(new.organism),
          "/",
          stringr::str_to_title(string = new.organism, locale = "en"),
          ".",
          assembly_option,
          ".gtf.gz"
        )
      assembly_is_correct <- exists.ftp.file.new(ensembl.qry, ensembl.qry)
      if (assembly_is_correct) {
        rest_api_status$release_coord_system_version <- assembly_option
        break
      }
    }
    # construct retrieval query

    local_file <- file.path(
      path,
      paste0(
        stringr::str_to_title(string = new.organism,
                              locale = "en"),
        ".",
        rest_api_status$release_coord_system_version,
        "_ensembl", ".gtf.gz"))
    if (file.exists(local_file)) {
            message("File ", local_file, " exists already.",
              " Thus, download has been skipped.")
    } else {
      if (rest_api_status$release_coord_system_version == "not_found") {
        message("Found organism but given release number did not specify existing file
                     in ensembl, maybe it is too old? Check that it exists on ensembl
                     first at all.")
        return(FALSE)
      }

      tryCatch({
              custom_download(ensembl.qry, destfile = local_file, mode = "wb")
      }, error = function(e)
              message(
                      "Something went wrong while trying to reach the file '",ensembl.qry,"'. This could be due to an instable internet connection or incorrect file path on the ENSEMBL ftp server. Please check if you are able to reach '",ensembl.qry, "' in your web browser.",
                      " In some cases ENSEMBL released a new database version and path names or the API weren't updated yet. Please give it a few days time or contact the ENSEMBL helpdesk."
              ))
    }

    return(local_file)
}
