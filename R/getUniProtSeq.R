#' @title Interface function to UniProt
#' @noRd
getUniProtSeq <-
  function(organism,
           path = NULL,
           gunzip = FALSE,
           update = FALSE,
           mute_citation = FALSE) {
    organism_new <- stringr::str_replace_all(organism, " ", "%20")
    organism_name_path <-
      stringr::str_replace_all(organism, " ", "_")

    message("-> Retrieve UniProt information for organism: ", organism)
    # retrieve all uniprot info for this organism
    uniprot_species_info <- getUniProtInfo(organism, update = update)

    if (nrow(uniprot_species_info) > 0) {
      # TODO: Generalize this part too
      download_file <-
        paste0(uniprot_species_info$upid,
               "_",
               uniprot_species_info$taxonomy,
               ".fasta.gz")

      # arachea
      kingdom <- uniprot_species_info$superregnum
      proteom_url <- "https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes"
      query <- file.path(proteom_url,
                         stringr::str_to_title(kingdom),
                         uniprot_species_info$upid,
                         download_file)


      local_proteome <- ifelse(is.null(path),
                               file.path(tempdir(), download_file),
                               file.path(path, download_file))
      custom_download(url = query, destfile = local_proteome)

      organism_fasta_file <- tryCatch({
        Biostrings::readBStringSet(local_proteome)
      }, error = function(e)
        message(
          "Something went wrong when trying to access the Uniprot API .Thus the query ",
          query,
          ". could not be used to retrieve a fasta file for '",
          organism,
          "'. Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall?"
        ))

      path_or_wd <- ifelse(is.null(path), getwd(), path)
      local_file <- file.path(path_or_wd,
                              paste0(organism_name_path,
                                     "_protein_uniprot.faa.gz"))

      message("-> Write downloaded *.fasta file to local disk ...")

      Biostrings::writeXStringSet(
        organism_fasta_file,
        filepath = local_file,
        compress = TRUE
      )

      docFile(
        file.name = basename(local_file),
        organism  = uniprot_species_info$name,
        url       = query,
        database  = "uniprot",
        path      = path_or_wd,
        refseq_category = "",
        assembly_accession = "",
        bioproject = "",
        biosample = "",
        taxid = uniprot_species_info$taxonomy,
        infraspecific_name = "",
        version_status = "",
        release_type = "",
        genome_rep = "",
        seq_rel_date = "",
        submitter = ""
      )


      gunzip_and_check(local_file, gunzip, format = "proteome",
                       mute_citation = mute_citation)
    } else {
      warning(
        "Unfortunately, no entry for '",
        organism,
        "' has been found. Are you certain that you typed the scientific name correctly, e.g. Homo sapiens (capital letter in the first name)?",
        call. = FALSE
      )
      return(NA)
  }
}
