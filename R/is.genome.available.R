#' @title Check Genome Availability
#' @description This function checks the availability of a given genome on the
#' NBCI servers specified as scientific name.
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' \item \code{db = "uniprot"}
#' }
#' @param organism there are three options to characterize an organism:
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @param details a logical value specifying whether or not details on genome
#' size, kingdom, etc. shall be printed to the console intead of a
#' boolean value.
#' @details
#' Internally this function calls the \code{\link{listGenomes}} function to
#' detect all available genomes and checks whether or not the specified organism
#' is available for download.
#' @return a logical value specifing whether or not the genome of the input
#' organism is available. In case \code{details} = \code{TRUE} only a character
#' string specifying the genome details is being returned.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#' # checking whether the Homo sapiens genome is stored on NCBI
#' is.genome.available(organism = "Homo sapiens", db = "refseq")
#'
#' # and printing details
#' is.genome.available(organism = "Homo sapiens", db = "refseq", details = TRUE)
#'
#' # checking whether the Homo sapiens genome is stored on ENSEMBL
#' is.genome.available(organism = "Homo sapiens", db = "ensembl")
#'
#' # and printing details
#' is.genome.available(organism = "Homo sapiens",
#'                     details = TRUE,
#'                     db = "ensembl")
#' }
#' @export

is.genome.available <- function(db = "refseq", organism,
                                skip_bacteria = "TRUE", details = FALSE) {
        all_db <- c(
          "refseq",
          "genbank",
          "ensembl",
          "ensemblgenomes",
          "uniprot"
        )

        if (!is.element(db, all_db))
            stop(
                "Please select one of the available data bases:\n",
                paste(all_db, collapse = ", "),
                call. = FALSE
            )

        if (is.element(db, c("refseq", "genbank"))) {
          return(is.genome.available.refseq.genbank(db = db, organism = organism, details = details))
        }

        if (db %in% c("ensembl", "ensemblgenomes")) {
          return(is.genome.available.ensembl(db, organism, details))
        }

        if (db == "uniprot") {

            if (is.taxid(organism)) {
                uniprot_rest_url <- paste0(
                    "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&taxid=",
                    as.integer(organism)
                )

                rest_status_test <- curl_fetch_memory(uniprot_rest_url)

                if (rest_status_test$status_code != 200) {
                    message(
                        "Something went wrong when trying to access the API 'https://www.ebi.ac.uk/proteins/api/proteomes'.",
                        " Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall? ", "Is it possbile to access the homepage 'https://www.ebi.ac.uk/' through your browser?"
                    )
                }
                uniprot_species_info <-
                    tibble::as_tibble(jsonlite::fromJSON(
                        uniprot_rest_url
                    ))

            } else {

                organism_new <- stringr::str_replace_all(organism, " ", "%20")

                uniprot_rest_url_name <- paste0(
                    "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&name=",
                    organism_new
                )

                rest_status_test_name <- curl_fetch_memory(uniprot_rest_url_name)

                if ((rest_status_test_name$status_code != 200)) {
                    message(
                        "Something went wrong when trying to access the API 'https://www.ebi.ac.uk/proteins/api/proteomes'.",
                        " Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall? ", "Is it possbile to access the homepage 'https://www.ebi.ac.uk/' through your browser?"
                    )
                }

                if (rest_status_test_name$status_code == 200) {
                    uniprot_species_info <-
                        tibble::as_tibble(jsonlite::fromJSON(
                            uniprot_rest_url_name
                        ))
                }
            }

            if (!details) {
                if (nrow(uniprot_species_info) == 0) {
                    message(
                        "Unfortunatey, no entry for '",
                        organism,
                        "' was found in the '",
                        db,
                        "' database. ",
                        "Please consider specifying ",
                        paste0("'db = ", dplyr::setdiff(
                            all_db,
                            db
                        ), collapse = "' or "),
                        "' to check whether '",
                        organism,
                        "' is available in these databases."
                    )
                    return(FALSE)
                }

                if (nrow(uniprot_species_info) > 0) {
                    message(
                        "A reference or representative genome assembly is available for '",
                        organism,
                        "'."
                    )
                    if (nrow(uniprot_species_info) > 1) {
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
                    return(TRUE)
                }

            }
          if (details)
            return(uniprot_species_info)
        }
}


is.genome.available.refseq.genbank <- function(db = "refseq",
                                               organism,
                                               skip_bacteria = TRUE,
                                               details = FALSE) {

  organism_name <- assembly_accession <- taxid <- NULL

  AssemblyFilesAllKingdoms <- getKingdomAssemblySummary(db = db, skip_bacteria = skip_bacteria)

  orgs <-
    stringr::str_replace_all(AssemblyFilesAllKingdoms$organism_name,
                             "\\(", "")
  orgs <- stringr::str_replace_all(orgs, "\\)", "")

  AssemblyFilesAllKingdoms <-
    dplyr::mutate(AssemblyFilesAllKingdoms, organism_name = orgs)

  organism <-
    stringr::str_replace_all(organism, "\\(", "")
  organism <-
    stringr::str_replace_all(organism, "\\)", "")

  # test if organism specification could be a taxid
  if (!is.taxid(organism)) {
    FoundOrganism <-
      dplyr::filter(AssemblyFilesAllKingdoms,
                    stringr::str_detect(organism_name, organism) |
                      assembly_accession == organism)

  } else {
    FoundOrganism <-
      dplyr::filter(AssemblyFilesAllKingdoms,
                    taxid == as.integer(organism))
  }

  if (nrow(FoundOrganism) == 0) {
    organism_no_hit_message_zero(organism, db)
    return(FALSE)
  }

  if (nrow(FoundOrganism) > 0) {
    if (!details) {

      if (all(FoundOrganism$refseq_category == "na")) {
        message("Only a non-reference genome assembly is available for '", organism, "'.",
                " Please make sure to specify the argument 'reference = FALSE' when running any get*() function.")
      } else {
        message("A reference or representative genome assembly is available for '", organism, "'.")
      }

      if (nrow(FoundOrganism) > 1) {
        organism_no_hit_message_more_than_one(organism, db)
      }
      return(TRUE)
    }


    if (details) {
      if (all(FoundOrganism$refseq_category == "na")) {
        message("Only a non-reference genome assembly is available for '", organism, "'.",
                " Please make sure to specify the argument 'reference = FALSE' when running any get*() function.")
      }
      return(FoundOrganism)
    }
  }
}

#' Check if genome is available in ensembl
#'
#' @param division "EnsemblVertebrates", alternatives: "EnsemblPlants", "EnsemblFungi"
#' @noRd
is.genome.available.ensembl <- function(db = "ensembl", organism,
                                        details = FALSE, divisions = ensembl_divisions()) {
  name <- accession <- accession <- assembly <- taxon_id <- NULL
  new.organism <- stringr::str_replace_all(organism, " ", "_")

  # For each ensembl division, check if it exists
  for (division in ensembl_divisions()) {
    ensembl.available.organisms <- get.ensembl.info(division = division)
    ensembl.available.organisms <- dplyr::filter(ensembl.available.organisms, !is.na(assembly))

    if (!is.taxid(organism)) {
      selected.organism <-
        dplyr::filter(
          ensembl.available.organisms,
          stringr::str_detect(name,
                              stringr::str_to_lower(new.organism)) |
            accession == organism, !is.na(assembly)
        )
    } else {
      selected.organism <-
        dplyr::filter(
          ensembl.available.organisms, taxon_id == as.integer(organism), !is.na(assembly))

    }
    if (nrow(selected.organism) > 0) break
  }



  if (!details) {
    if (nrow(selected.organism) == 0) {
      organism_no_hit_message_zero(organism, db)
      return(FALSE)
    }
    if (nrow(selected.organism) > 0) {
      message("A reference or representative genome assembly is available for '", organism, "'.")
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
      c("refseq", "genbank", "ensembl", "ensemblgenomes", "uniprot"), db
    ), collapse = "' or "),
    "' to check whether '",organism,"' is available in these databases."
  )
}

organism_no_hit_message_more_than_one <- function(organism, db) {
  message("More than one entry was found for '", organism, "'.",
          " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
          organism, ", db = ",db, ", details = TRUE)'.",
          " This will allow you to select the 'assembly_accession' identifier that can then be ",
          "specified in all get*() functions.")
}


