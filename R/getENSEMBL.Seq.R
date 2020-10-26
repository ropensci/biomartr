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

    new.organism <- ensembl_summary$name[1]
    new.organism <-
        paste0(
            stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
            stringr::str_sub(new.organism, 2, nchar(new.organism))
        )


    rest_url <- paste0(
        "http://rest.ensembl.org/info/assembly/",
        new.organism,
        "?content-type=application/json"
    )

    rest_api_status <- test_url_status(url = rest_url, organism = organism)
    if (is.logical(rest_api_status)) {
        return(FALSE)
    }

    release_api <- jsonlite::fromJSON(
            "http://rest.ensembl.org/info/data/?content-type=application/json"
    )$releases

    if (!is.null(release)){
        release <- as.numeric(release)

        if (!is.element(release, seq_len(as.integer(release_api))))
            stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
    }

    # construct retrieval query
    if (is.null(release)) {
        core_path <- "ftp://ftp.ensembl.org/pub/current_fasta/"
    } else {
        core_path <- paste0("ftp://ftp.ensembl.org/pub/release-", release ,"/fasta/")
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

    # Go through all possible assemblies, from newest to oldest, only 1 will match!
    rest_api_status$release_coord_system_version <- "not_found"
    for (assembly_option in all_possible_assemblies) {
        ensembl.qry <-
            paste0(
                    core_path,
                stringr::str_to_lower(new.organism),
                "/",
                type,
                "/",
                paste0(
                    stringr::str_to_title(new.organism, locale = "en"),
                    ".",
                    assembly_option,
                    ".",
                    type,
                    ifelse(id.type == "none","","."),
                    ifelse(id.type == "none","",id.type),
                    ".fa.gz"
                )
            )
        assembly_is_correct <- exists.ftp.file.new(ensembl.qry, ensembl.qry)
        if (assembly_is_correct) {
            rest_api_status$release_coord_system_version <- assembly_option
            break
        }
    }


    if (file.exists(file.path(
        path,
        paste0(
            stringr::str_to_title(new.organism, locale = "en"),
            ".",
            rest_api_status$release_coord_system_version,
            ".",
            type,
            ifelse(id.type == "none","","."),
            ifelse(id.type == "none","",id.type),
            ".fa.gz"
        )
    ))) {
        message("File ",file.path(
            path,
            paste0(
                stringr::str_to_title(new.organism, locale = "en"),
                ".",
                rest_api_status$release_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none","","."),
                ifelse(id.type == "none","",id.type),
                ".fa.gz"
            )
        )," exists already. Thus, download has been skipped.")
        return(file.path(
            path,
            paste0(
                stringr::str_to_title(new.organism, locale = "en"),
                ".",
                rest_api_status$release_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none","","."),
                ifelse(id.type == "none","",id.type),
                ".fa.gz"
            )
        ))
    } else {
        if (rest_api_status$release_coord_system_version == "not_found") {
            message("Found organism but given release number did not specify existing file
                     in ensembl, maybe it is too old? Check that it exists on ensembl
                     first at all.")
            return(FALSE)
        }

        custom_download(url = ensembl.qry,
                        destfile = file.path(
                            path,
                            paste0(
                                new.organism,
                                ".",
                                rest_api_status$release_coord_system_version,
                                ".",
                                type,
                                ifelse(id.type == "none", "", "."),
                                ifelse(id.type == "none", "", id.type),
                                ".fa.gz"
                            )
                        ),
                        mode = "wb")

        return(c(file.path(
            path,
            paste0(
                stringr::str_to_title(new.organism, locale = "en"),
                ".",
                rest_api_status$release_coord_system_version,
                ".",
                type,
                ifelse(id.type == "none", "", "."),
                ifelse(id.type == "none", "", id.type),
                ".fa.gz"
            )
        ), ensembl.qry))
    }
}
