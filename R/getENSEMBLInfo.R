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

validate_release <- function(release, ensembl_summary) {
  if (!is.null(release)) {
    release <- as.numeric(release)
    if (!is.element(release, ensembl_all_releases()))
      stop("Please provide a release number that is supported by ENSEMBL.", call. = FALSE)
  } else release <- ensembl_current_release(ensembl_summary$division[1])

  if (is.numeric(release)) {
    if (release <= 46) {
      message("ensembl release <= 46 is not supported")
      return(FALSE)
    }
  }
  return(release)
}

#' Check if genome is available in ensembl
#' @param organism which organism, scientific name
#' @param details logical, default FALSE, return logical only, else table of info
#' @param division "EnsemblVertebrates", alternatives: "EnsemblPlants",
#' "EnsemblFungi", "EnsemblMetazoa" and "EnsemblProtists"
#' @return logical, if details is TRUE, then returns table of details.
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

collection_table <- function(division = "EnsemblBacteria") {
  base_name_file <- paste0(division, ".txt")
  local_file <- file.path(tempdir(), base_name_file)

  if (!file.exists(local_file)) {
    url <- paste0(ensembl_ftp_server_url(division),"/",
                  ensembl_ftp_server_url_release(division),
                  "species_", base_name_file)
    tryCatch({
      custom_download(
        url,
        destfile = local_file,
        mode = "wb"
      )
    }, error = function(e) {
      message(
        "Something went wrong when accessing the the file",
        " Are you connected to the internet? ",
        "Is the homepage '", url,"' ",
        "currently available? Could it be that the scientific name is mis-spelled or includes special characters such as '.' or '('?"
      )
    })
  }
  suppressWarnings(
    collection <-
      readr::read_delim(
        local_file,
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

get_collection_id <- function(ensembl_summary) {
  division <- ensembl_summary$division[1]
  if (!(division %in% c("EnsemblBacteria", "EnsemblFungi",
                                           "EnsemblProtists")))
    return("") # Only these have collection folder structure

  get.org.info <- ensembl_summary[1,]
  collection_info <- collection_table(division)
  assembly <- NULL
  collection_info <-
    dplyr::filter(collection_info,
                  assembly == gsub("_$", "", get.org.info$assembly))

  if (nrow(collection_info) == 0) {
    message(
      "Unfortunately organism '",
      ensembl_summary$display_name,
      "' could not be found. Have you tried another database yet? ",
      "E.g. db = 'ensembl'? Thus, download for this species is omitted."
    )
    return(FALSE)
  }

  if (is.na(collection_info$core_db[1]) || collection_info$core_db[1] == "N") {
    # TODO make sure this is safe
    # In theory this should mean that the file exist outside collection folders
    return("")
  }
  collection <- paste0(paste0(unlist(
    stringr::str_split(collection_info$core_db[1], "_")
  )[1:3], collapse = "_"), "/")

  return(collection)
}


assembly_summary_and_rest_status <- function(organism) {
  ensembl_summary <- ensembl_assembly_hits(organism)
  if (isFALSE(ensembl_summary)) return(FALSE)

  # Check if assembly can be reached (TODO: remove, as this is already done)
  new.organism <- ensembl_proper_organism_name(ensembl_summary)
  rest_url <- ensembl_rest_url_assembly(new.organism)
  rest_api_status <- test_url_status(url = rest_url, organism = organism)
  if (isFALSE(rest_api_status)) return(FALSE)
  json.qry.info <- rest_api_status
  return(list(new.organism = new.organism,
              ensembl_summary = ensembl_summary,
              json.qry.info = json.qry.info))
}

write_assembly_docs_ensembl <- function(genome.path, new.organism, db, json.qry.info,
                                        path = dirname(genome.path[1]), append = NULL) {
  # generate Genome documentation
  sink(file.path(path, paste0("doc_", new.organism, "_db_", db, append,".txt")))

  cat(paste0("File Name: ", genome.path[1]))
  cat("\n")
  cat(paste0("Download Path: ", genome.path[2]))
  cat("\n")
  cat(paste0("Organism Name: ", new.organism))
  cat("\n")
  cat(paste0("Database: ", db))
  cat("\n")
  cat(paste0("Download_Date: ", date()))
  cat("\n")
  cat(paste0("assembly_name: ", json.qry.info$assembly_name))
  cat("\n")
  cat(paste0("assembly_date: ", json.qry.info$assembly_date))
  cat("\n")
  cat(
    paste0(
      "genebuild_last_geneset_update: ",
      json.qry.info$genebuild_last_geneset_update
    )
  )
  cat("\n")
  cat(paste0(
    "assembly_accession: ",
    json.qry.info$assembly_accession
  ))
  cat("\n")
  cat(
    paste0(
      "genebuild_initial_release_date: ",
      json.qry.info$genebuild_initial_release_date
    )
  )

  sink()

  doc <- tibble::tibble(
    file_name = genome.path[1],
    download_path = genome.path[2],
    organism = new.organism,
    database = db,
    download_data = date(),
    assembly_name = ifelse(!is.null(json.qry.info$assembly_name), json.qry.info$assembly_name, "none"),
    assembly_date = ifelse(!is.null(json.qry.info$assembly_date), json.qry.info$assembly_date, "none"),
    genebuild_last_geneset_update = ifelse(!is.null(json.qry.info$genebuild_last_geneset_update), json.qry.info$genebuild_last_geneset_update, "none"),
    assembly_accession = ifelse(!is.null(json.qry.info$assembly_accession), json.qry.info$assembly_accession, "none"),
    genebuild_initial_release_date = ifelse(!is.null(json.qry.info$genebuild_initial_release_date), json.qry.info$genebuild_initial_release_date, "none")

  )

  readr::write_tsv(doc, file = file.path(
    path,
    paste0("doc_", new.organism, "_db_", db, append, ".tsv"))
  )
  return(invisible(NULL))
}
