#' @title Helper function for retrieving biological sequence files from ENSEMBL
#' @description This function downloads gff files of query
#' organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type biological sequence type.
#' @param id.type a character, default "toplevel". id type of assembly, either toplevel or primary_assembly usually.
#' @param release a numeric, the database release version of ENSEMBL (\code{db = "ensembl"}). Default is \code{release = NULL} meaning
#' that the most recent database version is used. \code{release = 75} would for human would give the stable
#' GRCh37 release in ensembl. Value must be > 46, since ensembl did not structure their data
#' if the standard format before that.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @return either a character path to downloaded file, or a logical FALSE, specifying failure.
getENSEMBL.Seq <- function(organism, type = "dna", id.type = "toplevel", release = NULL, path) {

    if (!is.element(type, c("dna", "cds", "pep", "ncrna")))
        stop("Please a 'type' argument supported by this function:
             'dna', 'cds', 'pep', 'ncrna'.")


    ensembl_summary <-
        suppressMessages(is.genome.available(
            organism = organism,
            db = "ensembl",
            details = TRUE
        ))

    if (nrow(ensembl_summary) == 0) {
        message("Unfortunately, organism '",organism,"' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted.")
        return(FALSE)
    }

    taxon_id <- assembly <- name <- accession <- NULL

    if (nrow(ensembl_summary) > 1) {
        if (is.taxid(organism)) {
            ensembl_summary <-
                dplyr::filter(ensembl_summary, taxon_id == as.integer(organism), !is.na(assembly))
        } else {

            ensembl_summary <-
                dplyr::filter(
                    ensembl_summary,
                    (name == stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))) |
                        (accession == organism),
                        !is.na(assembly)
                )
        }
    }
    # Check if assembly can be reached
    new.organism <- ensembl_proper_organism_name(ensembl_summary)
    rest_url <- ensembl_rest_url_assembly(new.organism)
    rest_api_status <- test_url_status(url = rest_url, organism = organism)
    if (is.logical(rest_api_status)) {
        return(FALSE)
    }

    if (!is.null(release)) {
        release <- as.numeric(release)
        if (!is.element(release, ensembl_all_releases()))
            stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
    }

    # construct retrieval query
    # Before 75, ensembl used .release extension on assembly
    all_possible_assemblies <- rest_api_status$coord_system_versions
    if (is.numeric(release)) {
        if (release <= 46) {
            message("ensembl release <= 46 is not supported")
            return(FALSE)
        }
        if (release <= 75) {
            all_possible_assemblies <- paste0(all_possible_assemblies,
                                              ".", release)
        }
    }

    # construct retrieval query
    core_path <- ensembl_ftp_server_url_fasta(ensembl_summary$division[1],
                                              release)
    # Go through all possible assemblies, from newest to oldest, only 1 will match!
    rest_api_status$release_coord_system_version <- "not_found"
    for (assembly_option in all_possible_assemblies) {
        ensembl.qry <- ensembl_ftp_server_query_full(core_path, new.organism,
                                              type, assembly_option, id.type)

        assembly_is_correct <- exists.ftp.file.new(ensembl.qry, ensembl.qry)
        if (assembly_is_correct) {
            rest_api_status$release_coord_system_version <- assembly_option
            break
        }
    }

    local_file <- ensembl_seq_local_path(path, new.organism, rest_api_status,
                                         type, id.type)
    if (file.exists(local_file)) {
        message("File ",local_file,
                " exists already. Thus, download has been skipped.")
        return(local_file)
    } else {
        if (rest_api_status$release_coord_system_version == "not_found") {
            message("Found organism but given release number did not specify existing file
                     in ensembl, maybe it is too old? Check that it exists on ensembl
                     first at all.")
            return(FALSE)
        }

        custom_download(url = ensembl.qry,
                        destfile = local_file,
                        mode = "wb")

        return(c(local_file, ensembl.qry))
    }
}

ensembl_seq_file_base <- function(new.organism, assembly_option, type,
                                  id.type) {
  paste0(
    stringr::str_to_title(new.organism, locale = "en"),
    ".",
    assembly_option,
    ".",
    type,
    ifelse(id.type == "none", "", paste0(".", id.type)),
    ".fa.gz"
  )
}
ensembl_seq_local_path <- function(path, new.organism, rest_api_status,
                                   type, id.type) {
  assembly_option <- rest_api_status$release_coord_system_version
  file.path(
    path,
    ensembl_seq_file_base(new.organism, assembly_option, type,
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

ensembl_fix_wrong_naming <- function(organism) {
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
}

ensembl_proper_organism_name <- function(ensembl_summary) {
  new.organism <- ensembl_summary$name[1]
  new.organism <-
    paste0(
      stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
      stringr::str_sub(new.organism, 2, nchar(new.organism))
    )
  new.organism <- ensembl_fix_wrong_naming(new.organism)
}
