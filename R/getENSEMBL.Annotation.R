#' @title Helper function for retrieving gff/gtf files from ENSEMBL
#' @description This function downloads gff/gtff
#' files of query organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type specification type.
#' @param release the ENSEMBL release. Default is \code{release = NULL} meaning that the current (most recent) version is used.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBL.Annotation <-
    function(organism,
             type = "dna",
             release = NULL,
             path, format = "gff3") {

      if (!is.element(type, c("dna", "cds", "pep")))
          stop("Please a 'type' argument supported by this function:
               'dna', 'cds', 'pep'.")
      annotation_formats <- c("gff3", "gtf")
      if (!is.element(format, annotation_formats))
        stop("Please a 'format' argument supported by this function: ",
             paste(annotation_formats, collapse = ", "),".")

      ensembl_summary <- ensembl_assembly_hits(organism)
      if (isFALSE(ensembl_summary)) return(FALSE)

      # Check if assembly can be reached
      new.organism <- ensembl_proper_organism_name(ensembl_summary)
      rest_url <- ensembl_rest_url_assembly(new.organism)
      rest_api_status <- test_url_status(url = rest_url, organism = organism)
      if (is.logical(rest_api_status)) {
          return(FALSE)
      }

      release <- validate_release(release, ensembl_summary)

      # construct retrieval query
      # Before 75, ensembl used .release extension on assembly
      # TODO: check this is valid
      all_possible_assemblies <- rest_api_status$coord_system_versions
      all_possible_assemblies <- paste0(all_possible_assemblies,
                                        ".", release)
      core_path <- ensembl_ftp_server_url_gtf(ensembl_summary$division[1],
                                              release, format = format)
      # construct retrieval query
      rest_api_status$release_coord_system_version <- "not_found"
      for (assembly_option in all_possible_assemblies) {
        collection_folder <- get_collection_id(ensembl_summary)
        if (isFALSE(collection_folder)) return(FALSE)
        ensembl.qry <-
          paste0(
            core_path,
            collection_folder,
            stringr::str_to_lower(new.organism),
            "/",
            stringr::str_to_title(string = new.organism, locale = "en"),
            ".",
            assembly_option,
            ".", format, ".gz"
          )
        assembly_is_correct <- exists.ftp.file.new(ensembl.qry, ensembl.qry)
        if (assembly_is_correct) {
          rest_api_status$release_coord_system_version <- assembly_option
          break
        }
      }

      local_file <- file.path(
        path,
        paste0(
          stringr::str_to_title(string = new.organism,
                                locale = "en"),
          ".",
          rest_api_status$release_coord_system_version,
          "_ensembl", ".", format, ".gz"))
      custom_download_check_local(ensembl.qry, local_file, rest_api_status)

      return(c(local_file, ensembl.qry))
    }

#' @title Helper function for retrieving gff files from ENSEMBL
#' @description This function downloads gff
#' files of query organisms from ENSEMBL.
#' @inheritParams getENSEMBL.Seq
#' @import curl RCurl
#' @author Hajk-Georg Drost
#' @return character filepath to download file, returns FALSE if failed.
getENSEMBL.gtf <- function(organism, type = "dna",
                           path, release = NULL) {
  getENSEMBL.Annotation(organism, type = type,
                        release = release,
                        path = path, format = "gtf")[1]
}
