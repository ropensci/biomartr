#' @title Retrieve ENSEMBL info file
#' @description Retrieve species and genome information from
#' http://rest.ensembl.org/info/species?content-type=application/json/.
#' @author Hajk-Georg Drost
#' @return a tibble table storing info for all available ENSEMBL divisions.
#' @examples
#' \dontrun{
#' # look at available divisions
#' ensembl_divisions()
#' # retrieve information for all ENSEMBL divisions at once
#' test <- getENSEMBLInfo()
#' test
#' # retrieve information for a particular ENSEMBL division (e.g. EnsemblVertebrates)
#' test_vertebrates <- get.ensembl.info(update = TRUE, division = "EnsemblVertebrates")
#' test_vertebrates
#' }
#' @seealso \code{\link{ensembl_divisions}}, \code{\link{get.ensembl.info}}, \code{\link{getKingdomAssemblySummary}}
#' @export
getENSEMBLInfo <- function() {
  all_divisions <- ensembl_divisions()
  ENSEMBLInfoTable <- vector("list", length(all_divisions))

  for (i in seq_len(length(all_divisions))) {
    cat("Starting information retrieval for:", all_divisions[i])
    cat("\n")
    ENSEMBLInfoTable[[i]] <-
      get.ensembl.info(update = TRUE, division = all_divisions[i])
  }

  return(dplyr::bind_rows(ENSEMBLInfoTable))
}

ensembl_assembly_hits <- function(organism) {
  ensembl_summary <-
    suppressMessages(is.genome.available.ensembl(
      organism = organism,
      db = "ensembl",
      details = TRUE
    ))

  if (nrow(ensembl_summary) == 0) {
    message(
      "Unfortunately, organism '",
      organism,
      "' does not exist in this database. ",
      "Could it be that the organism name is misspelled? Thus, download has been omitted."
    )
    return(FALSE)
  }
  ensembl_summary <-
    ensembl_summaries_filter(ensembl_summary, organism)
  return(ensembl_summary)
}

ensembl_summaries_filter <- function(ensembl_summary, organism) {
  if (nrow(ensembl_summary) > 1) {
    taxon_id <- assembly <- name <- accession <- NULL
    ensembl_summary_copy <- ensembl_summary
    if (is.taxid(organism)) {
      ensembl_summary <-
        dplyr::filter(ensembl_summary,
                      taxon_id == as.integer(organism),
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
        ensembl_summary <-
          dplyr::filter(ensembl_summary_copy, !is.na(assembly))
      }
    }
    if (nrow(ensembl_summary) == 0) {
      print(ensembl_summary_copy)
      stop("All assemblies removed by filter (more info in lines above)")
    }
  }
  return(ensembl_summary)
}

is.taxid <- function(x) {
  return(stringr::str_count(x, "[:digit:]") == nchar(x))
}

#' Check if genome is available in ensembl
#'
#' @param division "EnsemblVertebrates", alternatives: "EnsemblPlants", "EnsemblFungi"
#' @noRd
is.genome.available.ensembl <- function(db = "ensembl",
                                        organism,
                                        details = FALSE,
                                        divisions = ensembl_divisions()) {
  name <- accession <- accession <- assembly <- taxon_id <- NULL
  new.organism <- stringr::str_replace_all(organism, " ", "_")

  # For each ensembl division, check if it exists
  for (division in ensembl_divisions()) {
    ensembl.available.organisms <- get.ensembl.info(division = division)
    ensembl.available.organisms <-
      dplyr::filter(ensembl.available.organisms, !is.na(assembly))

    if (!is.taxid(organism)) {
      selected.organism <-
        dplyr::filter(
          ensembl.available.organisms,
          stringr::str_detect(name,
                              stringr::str_to_lower(new.organism)) |
            accession == organism,!is.na(assembly)
        )
    } else {
      selected.organism <-
        dplyr::filter(
          ensembl.available.organisms,
          taxon_id == as.integer(organism),!is.na(assembly)
        )

    }
    if (nrow(selected.organism) > 0)
      break
  }



  if (!details) {
    if (nrow(selected.organism) == 0) {
      organism_no_hit_message_zero(organism, db)
      return(FALSE)
    }
    if (nrow(selected.organism) > 0) {
      message("A reference or representative genome assembly is available for '",
              organism,
              "'.")
      if (nrow(selected.organism) > 1) {
        organism_no_hit_message_more_than_one(organism, db)
      }
      return(TRUE)
    }
  }
  if (details)
    return(selected.organism)
}

organism_no_hit_message_zero <- function(organism, db) {
  message(
    "Unfortunatey, no entry for '",
    organism,
    "' was found in the '",
    db,
    "' database. ",
    "Please consider specifying ",
    paste0("'db = ", dplyr::setdiff(
      c("refseq", "genbank", "ensembl", "ensemblgenomes", "uniprot"),
      db
    ), collapse = "' or "),
    "' to check whether '",
    organism,
    "' is available in these databases."
  )
}

organism_no_hit_message_more_than_one <- function(organism, db) {
  message(
    "More than one entry was found for '",
    organism,
    "'.",
    " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
    organism,
    ", db = ",
    db,
    ", details = TRUE)'.",
    " This will allow you to select the 'assembly_accession' identifier that can then be ",
    "specified in all get*() functions."
  )
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
  if (ensembl_summary$division[1] != "EnsemblBacteria")
    return("")

  get.org.info <- ensembl_summary[1,]
  bacteria.info <- all_bacterias_info()
  assembly <- NULL
  bacteria.info <-
    dplyr::filter(bacteria.info,
                  assembly == gsub("_$", "", get.org.info$assembly))

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
