readAssemblyDoc <- function(path, db, file = list.files(path, "(^doc_.*)(.*tsv$)", full.names = TRUE)) {
  if (length(file) != 1) stop("More than one document to read, can only read 1!")
  if (is.element(db, c("refseq", "genbank"))) {
    tsv_file <-
      readr::read_tsv(
        file,
        col_types = readr::cols(
          file_name = readr::col_character(),
          organism = readr::col_character(),
          url = readr::col_character(),
          database = readr::col_character(),
          path = readr::col_character(),
          refseq_category = readr::col_character(),
          assembly_accession = readr::col_character(),
          bioproject = readr::col_character(),
          biosample = readr::col_character(),
          taxid = readr::col_integer(),
          infraspecific_name = readr::col_character(),
          version_status = readr::col_character(),
          release_type = readr::col_character(),
          genome_rep = readr::col_character(),
          seq_rel_date = readr::col_date(format = ""),
          submitter = readr::col_character()
        ),
        col_names = TRUE
      )
  } else if (is.element(db, c("ensembl"))) {
    tsv_file <-
      readr::read_tsv(
        file,
        col_types = readr::cols(
          file_name = readr::col_character(),
          organism = readr::col_character(),
          database = readr::col_character(),
          download_data = readr::col_character(),
          assembly_name = readr::col_character(),
          assembly_date = readr::col_character(),
          genebuild_last_geneset_update = readr::col_character(),
          assembly_accession = readr::col_character(),
          genebuild_initial_release_date = readr::col_character()
        ),
        col_names = TRUE
      )
  }
}

