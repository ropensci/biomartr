#' @title Download sequence or annotation from ENSEMBL
#' @description Backend function for retrieving files sequence and annotation
#' files from the ENSEMBL ftp server
#' @inheritParams getBio
#' @param type character, biological sequence type (e.g. "dna", "cds")
#' @param id.type a character, default "toplevel".
#' id type of assembly, either "toplevel" or "primary_assembly" for genomes.
#' Can be other strings, for non genome objects.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @return either a character path to downloaded file, or a logical FALSE, specifying failure.
getENSEMBL <- function(organism, type = "dna", id.type = "toplevel", release = NULL, path, format) {

  annotation_formats <- c("gff3", "gtf")
  all_format <- c(annotation_formats, "fasta")
  if (!is.element(format, all_format))
    stop("Please a 'format' argument supported by this function: ",
         paste(all_format, collapse = ", "),".")

  region_types <- c("dna", "cds", "pep", "ncrna")
  if (!is.element(type, region_types))
    stop("Please a 'type' argument supported by this function: ",
         paste(region_types, collapse = ", "),".")

  ensembl_summary <- ensembl_assembly_hits(organism)
  if (isFALSE(ensembl_summary)) return(FALSE)
  release <- validate_release(release, ensembl_summary)

  # Check if assembly can be reached
  new.organism <- ensembl_proper_organism_name(ensembl_summary)
  rest_url <- ensembl_rest_url_assembly(new.organism)
  rest_api_status <- test_url_status(url = rest_url, organism = organism)
  if (isFALSE(rest_api_status)) return(FALSE)


  # construct retrieval query
  # Before 75, ensembl used .release extension on assembly

  all_possible_assemblies <- rest_api_status$coord_system_versions

  # construct retrieval query
  kingdom <- ensembl_summary$division[1]
  core_path <- ensembl_ftp_server_url_format_full(kingdom, release, format)
  # Validate that final URL exists, if not, "not_found" is kept.
  rest_api_status$release_coord_system_version <- "not_found"
  for (assembly_option in all_possible_assemblies) {
    ensembl.qry <- ftp_url_ensembl(core_path, new.organism, assembly_option,
                                   ensembl_summary, format, type, id.type,
                                   release)
    if (!isFALSE(ensembl.qry)) {
      assembly_is_correct <- exists.ftp.file.new(ensembl.qry, ensembl.qry)
      if (any(assembly_is_correct)) {
        if (length(assembly_is_correct) > 1) {
          id.type <- id.type[assembly_is_correct][1]
          message("Auto detected assembly type: ", id.type)
          ensembl.qry <- ensembl.qry[assembly_is_correct][1]
        }
        rest_api_status$release_coord_system_version <- assembly_option
        break
      }
    }
  }

  local_file <- local_path_ensembl(path, new.organism, rest_api_status,
                                   format, type, release, id.type)

  custom_download_check_local(ensembl.qry, local_file, rest_api_status)
  return(c(local_file, ensembl.qry))
}

local_path_ensembl <- function(path, new.organism, rest_api_status,
                               format, type, release, id.type) {
  assembly <- rest_api_status$release_coord_system_version
  if (assembly == "not_found") return(FALSE)

  if (format == "fasta") {
    ensembl_seq_local_path(path, new.organism, assembly,
                           type, id.type)
  } else {
    file.path(
      path,
      paste0(ensembl_assembly_stem(new.organism, assembly),
             release, ".", "_ensembl",
             ".", format, ".gz"))
  }
}

#' Get the ftp relative file name of the assembly file wanted
#' @noRd
ftp_url_ensembl <- function(core_path, new.organism, assembly,
                            ensembl_summary, format, type, id.type = "none",
                            release = NULL) {

  dir <- ftp_url_ensembl_dir(core_path, ensembl_summary, new.organism, type)
  url_stem <- file.path(dir, ensembl_assembly_stem(new.organism, assembly))

  if (!is.null(type)) type <- paste0(type, ".")
  if (!is.null(id.type) && all(id.type == "none")) id.type <- NULL
  if (!is.null(id.type)) id.type <- paste0(id.type, ".")
  release. <- release
  is_fasta <- format == "fasta"
  old_fasta_naming <- is_fasta && is.numeric(release) && release <= 75

  if (!is_fasta | old_fasta_naming) {
    release. <- paste0(release, ".")
  } else release. <- NULL

  url <- paste0(url_stem, release.)
  if (is_fasta) {
    format <- "fa"
    url <- paste0(url_stem, type, id.type)
  }
  url <- paste0(url, format, ".gz")
  return(url)
}

#' Get the ftp directory of the assembly
#' @noRd
ftp_url_ensembl_dir <- function(core_path, ensembl_summary, new.organism, type) {
  collection_folder <- get_collection_id(ensembl_summary)
  if (isFALSE(collection_folder)) return(FALSE)
  dir <- paste0(core_path,
                collection_folder,
                stringr::str_to_lower(new.organism),
                "/")
  if (basename(core_path) == "fasta") dir <- paste0(dir, type, "/")
  return(dir)
}

ensembl_assembly_stem <- function(new.organism, assembly_option, suffix = ".") {
  paste0(
    stringr::str_to_title(new.organism),
    ".",
    assembly_option,
    suffix)
}

ensembl_seq_file_base <- function(new.organism, assembly_option, type,
                                  id.type) {
  paste0(
    ensembl_assembly_stem(new.organism, assembly_option),
    type,
    ifelse(id.type == "none", "", paste0(".", id.type)),
    ".fa.gz"
  )
}
ensembl_seq_local_path <- function(path, new.organism, assembly,
                                   type, id.type) {
  file.path(
    path,
    ensembl_seq_file_base(new.organism, assembly, type,
                          id.type)
  )
}

ensembl_gtf_local_path <- function(path, new.organism, rest_api_status,
                                   type, id.type) {
  assembly_option <- rest_api_status$release_coord_system_version
  file.path(
    path,
    ensembl_seq_file_base(new.organism, assembly_option, type,
                          id.type)
  )
}





