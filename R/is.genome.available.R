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

is.genome.available <-
    function(
             db = "refseq",
             organism,
             details = FALSE
             ) {

        if (!is.element(db, c("refseq", "genbank",
                              "ensembl", "uniprot", "ensemblgenomes")))
            stop(
                "Please select one of the available data bases:
                'refseq', 'genbank', 'ensembl', or 'uniprot', 'ensemblgenomes'",
                call. = FALSE
            )

        if (is.element(db, c("refseq", "genbank"))) {
          return(is.genome.available.refseq.genbank(db, organism, details))
        }

        if (db == "ensembl") {
          return(is.genome.available.ensembl(db, organism, details))
        }

        if (db == "ensemblgenomes") {
            new.organism <- stringr::str_replace_all(organism, " ", "_")

            name <- accession <- assembly <- taxon_id <- NULL

            ensembl.available.organisms <- get.ensemblgenome.info()
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

            if (!details) {

                if (nrow(selected.organism) == 0) {
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
                    return(FALSE)
                }

                if (nrow(selected.organism) > 0) {
                    message("A reference or representative genome assembly is available for '", organism, "'.")
                    if (nrow(selected.organism) > 1) {
                        message("More than one entry was found for '", organism, "'.",
                                " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = '",
                                organism, "', db = '",db, "', details = TRUE)'.",
                                " This will allow you to select the 'assembly_accession' identifier that can then be ",
                                "specified in all get*() functions.")
                    }
                    return(TRUE)
                }

            }

            if (details)
                return(selected.organism)
        }

        if (db == "uniprot") {

            if (is.taxid(organism)) {
                unipreot_rest_url <- paste0(
                    "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&taxid=",
                    as.integer(organism)
                )

                rest_status_test <- curl_fetch_memory(unipreot_rest_url)

                if (rest_status_test$status_code != 200) {
                    message(
                        "Something went wrong when trying to access the API 'https://www.ebi.ac.uk/proteins/api/proteomes'.",
                        " Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall? ", "Is it possbile to access the homepage 'https://www.ebi.ac.uk/' through your browser?"
                    )
                }
                uniprot_species_info <-
                    tibble::as_tibble(jsonlite::fromJSON(
                        unipreot_rest_url
                    ))

            } else {

                organism_new <- stringr::str_replace_all(organism, " ", "%20")

                unipreot_rest_url_name <- paste0(
                    "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&name=",
                    organism_new
                )

                rest_status_test_name <- curl_fetch_memory(unipreot_rest_url_name)


                unipreot_rest_url_upid <- paste0(
                    "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&upid=",
                    organism
                )

                rest_status_test_upid <- curl_fetch_memory(unipreot_rest_url_upid)

                if ((rest_status_test_upid$status_code != 200) & (rest_status_test_name$status_code != 200)) {
                    message(
                        "Something went wrong when trying to access the API 'https://www.ebi.ac.uk/proteins/api/proteomes'.",
                        " Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall? ", "Is it possbile to access the homepage 'https://www.ebi.ac.uk/' through your browser?"
                    )
                }

                if (rest_status_test_name$status_code == 200) {
                    uniprot_species_info <-
                        tibble::as_tibble(jsonlite::fromJSON(
                            unipreot_rest_url_name
                        ))
                }

                if (rest_status_test_upid$status_code == 200) {
                    uniprot_species_info <-
                        tibble::as_tibble(jsonlite::fromJSON(
                            unipreot_rest_url_upid
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
                            c(
                                "refseq",
                                "genbank",
                                "ensembl",
                                "ensemblgenomes",
                                "uniprot"
                            ),
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
  # if AssemblyFilesAllKingdoms.txt file was already
  # generated/downloaded then use the local version
  # stored in temp()
  if (file.exists(file.path(
    tempdir(),
    paste0("AssemblyFilesAllKingdoms_", db, ".txt")
  )) & skip_bacteria) {
    suppressWarnings(
      AssemblyFilesAllKingdoms <-
        readr::read_tsv(
          file.path(
            tempdir(),
            paste0("AssemblyFilesAllKingdoms_", db, ".txt")
          ),
          comment = "#",
          col_names = c(
            "assembly_accession",
            "bioproject",
            "biosample",
            "wgs_master",
            "refseq_category",
            "taxid",
            "species_taxid",
            "organism_name",
            "infraspecific_name",
            "isolate",
            "version_status",
            "assembly_level",
            "release_type",
            "genome_rep",
            "seq_rel_date",
            "asm_name",
            "submitter",
            "gbrs_paired_asm",
            "paired_asm_comp",
            "ftp_path",
            "excluded_from_refseq",
            "relation_to_type_material",
            "asm_not_live_date",
            "assembly_type",
            "group",
            "genome_size",
            "genome_size_ungapped",
            "gc_percent",
            "replicon_count",
            "scaffold_count",
            "contig_count",
            "annotation_provider",
            "annotation_name",
            "annotation_date",
            "total_gene_count",
            "protein_coding_gene_count",
            "non_coding_gene_count",
            "pubmed_id"
          ),
          col_types = readr::cols(
            assembly_accession = readr::col_character(),
            bioproject = readr::col_character(),
            biosample = readr::col_character(),
            wgs_master = readr::col_character(),
            refseq_category = readr::col_character(),
            taxid = readr::col_integer(),
            species_taxid = readr::col_integer(),
            organism_name = readr::col_character(),
            infraspecific_name = readr::col_character(),
            isolate = readr::col_character(),
            version_status = readr::col_character(),
            assembly_level = readr::col_character(),
            release_type = readr::col_character(),
            genome_rep = readr::col_character(),
            seq_rel_date = readr::col_date(),
            asm_name = readr::col_character(),
            submitter = readr::col_character(),
            gbrs_paired_asm = readr::col_character(),
            paired_asm_comp = readr::col_character(),
            ftp_path = readr::col_character(),
            excluded_from_refseq = readr::col_character(),
            relation_to_type_material = readr::col_character(),
            asm_not_live_date = readr::col_character(),
            assembly_type = readr::col_character(),
            group = readr::col_character(),
            genome_size = readr::col_integer(),
            genome_size_ungapped = readr::col_integer(),
            gc_percent = readr::col_double(),
            replicon_count = readr::col_integer(),
            scaffold_count = readr::col_integer(),
            contig_count = readr::col_integer(),
            annotation_provider = readr::col_character(),
            annotation_name = readr::col_character(),
            annotation_date = readr::col_date(format = "%m/%d/%y"),
            total_gene_count = readr::col_integer(),
            protein_coding_gene_count = readr::col_integer(),
            non_coding_gene_count = readr::col_integer(),
            pubmed_id = readr::col_character()
          )
        )
    )
  } else {
    # otherwise download all assembly_summary.txt files for all
    # kingdoms and store the AssemblyFilesAllKingdoms.txt
    # file locally
    # retrieve the assembly_summary.txt files for all kingdoms
    kgdoms <- getKingdoms(db = db)
    storeAssemblyFiles <- vector("list", length(kgdoms))

    for (i in seq_along(kgdoms)) {
      if ()
      storeAssemblyFiles[i] <-
        list(getSummaryFile(db = db, kingdom = kgdoms[i]))
    }

    AssemblyFilesAllKingdoms <-
      dplyr::bind_rows(storeAssemblyFiles)

    readr::write_tsv(AssemblyFilesAllKingdoms,
                     file.path(
                       tempdir(),
                       paste0("AssemblyFilesAllKingdoms_",
                              db, ".txt")
                     ))
  }

  organism_name <- assembly_accession <- taxid <- NULL

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


