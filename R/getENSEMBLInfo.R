#' @title Retrieve ENSEMBL info file
#' @description Retrieve species and genome information from
#' http://rest.ensembl.org/info/species?content-type=application/json/.
#' @author Hajk-Georg Drost
#' @export
getENSEMBLInfo <- function() {
    ENSEMBLInfoTable <- get.ensembl.info(update = TRUE)
    return(ENSEMBLInfoTable)
}

ensembl_assembly_hits <- function(organism) {
  ensembl_summary <-
    suppressMessages(is.genome.available(
      organism = organism,
      db = "ensembl",
      details = TRUE
    ))

  if (nrow(ensembl_summary) == 0) {
    message("Unfortunately, organism '",organism,"' does not exist in this database. ",
            "Could it be that the organism name is misspelled? Thus, download has been omitted.")
    return(FALSE)
  }
  ensembl_summary <- ensembl_summaries_filter(ensembl_summary, organism)
  return(ensembl_summary)
}

ensembl_summaries_filter <- function(ensembl_summary, organism) {
  if (nrow(ensembl_summary) > 1) {
    taxon_id <- assembly <- name <- accession <- NULL
    ensembl_summary_copy <- ensembl_summary
    if (is.taxid(organism)) {
      ensembl_summary <-
        dplyr::filter(ensembl_summary, taxon_id == as.integer(organism),
                      !is.na(assembly))
    } else {
      ensembl_summary <-
        dplyr::filter(
          ensembl_summary,
          (name == lower_cap_underscore_organism_name(organism)) |
            (accession == organism),
          !is.na(assembly)
        )
      if (nrow(ensembl_summary) == 0) {
        ensembl_summary <- dplyr::filter(ensembl_summary_copy,
                                         !is.na(assembly))
      }
    }
    if (nrow(ensembl_summary) == 0) {
      print(ensembl_summary_copy)
      stop("All assemblies removed by filter (more info in lines above)")
    }
  }
  return(ensembl_summary)
}

all_bacterias_info <- function() {
  if (!file.exists(file.path(tempdir(), "EnsemblBacteria.txt"))) {
    tryCatch({
      custom_download(
        "ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt",
        destfile = file.path(tempdir(), "EnsemblBacteria.txt"),
        mode = "wb"
      )
    }, error = function(e) {
      message(
        "Something went wrong when accessing the API 'http://rest.ensemblgenomes.org'.",
        " Are you connected to the internet? ",
        "Is the homepage 'ftp://ftp.ensemblgenomes.org/pub/current/bacteria/species_EnsemblBacteria.txt' ",
        "currently available? Could it be that the scientific name is mis-spelled or includes special characters such as '.' or '('?"
      )
    })
  }
  suppressWarnings(
    bacteria.info <-
      readr::read_delim(
        file.path(tempdir(), "EnsemblBacteria.txt"),
        delim = "\t",
        quote = "\"",
        escape_backslash = FALSE,
        col_names = c(
          "name",
          "species",
          "division",
          "taxonomy_id",
          "assembly",
          "assembly_accession",
          "genebuild",
          "variation",
          "microarray",
          "pan_compara",
          "peptide_compara",
          "genome_alignments",
          "other_alignments",
          "core_db",
          "species_id"
        ),
        col_types = readr::cols(
          name = readr::col_character(),
          species = readr::col_character(),
          division = readr::col_character(),
          taxonomy_id = readr::col_integer(),
          assembly = readr::col_character(),
          assembly_accession = readr::col_character(),
          genebuild = readr::col_character(),
          variation = readr::col_character(),
          microarray = readr::col_character(),
          pan_compara = readr::col_character(),
          peptide_compara = readr::col_character(),
          genome_alignments = readr::col_character(),
          other_alignments = readr::col_character(),
          core_db = readr::col_character(),
          species_id = readr::col_integer()
        ),
        comment = "#"
      )
  )
}

get_bacteria_collection_id <- function(ensembl_summary) {
  if (ensembl_summary$division[1] != "EnsemblBacteria") return("")

  get.org.info <- ensembl_summary[1,]
  bacteria.info <- all_bacterias_info()
  assembly <- NULL
  bacteria.info <-
    dplyr::filter(bacteria.info,
                  assembly == get.org.info$assembly)

  if (nrow(bacteria.info) == 0) {
    message(
      "Unfortunately organism '",
      ensembl_summary$display_name,
      "' could not be found. Have you tried another database yet? ",
      "E.g. db = 'ensembl'? Thus, download for this species is omitted."
    )
    return(FALSE)
  }

  if (is.na(bacteria.info$core_db[1])) {
    message(
      "Unfortunately organism '",
      ensembl_summary$display_name,
      "' was not assigned to a bacteria collection.
                    Thus download for this species is omitted."
    )
    return(FALSE)
  }
  bacteria_collection <- paste0(paste0(unlist(
    stringr::str_split(bacteria.info$core_db[1], "_")
  )[1:3], collapse = "_"), "/")

  return(bacteria_collection)
}
