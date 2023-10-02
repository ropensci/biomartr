#' @title Interface function to UniProt
#' @noRd
getUniProtSeq <-
  function(organism,
           path = NULL,
           gunzip = FALSE,
           update = FALSE) {
    organism_new <- stringr::str_replace_all(organism, " ", "%20")
    organism_name_path <-
      stringr::str_replace_all(organism, " ", "_")
    message("-> Retrieve UniProt information for organism: ", organism)
    # retrieve all uniprot info for this organism
    uniprot_species_info <- getUniProtInfo(organism = organism)
    
    name <-
      upid <-
      taxonomy <-
      isReferenceProteome <- isRepresentativeProteome <- NULL
    isRepresentativeProteome <- NULL
    
    uniprot_species_info <-
      dplyr::filter(uniprot_species_info, stringr::str_detect(name, organism))
    
    if (nrow(uniprot_species_info) > 1) {
      # message("There are more than one entry for '",organism,"'.",
      #         " Please select the one below that you prefer and re-run this function using the full name you chose.")
      # message("\n")
      # message("Options are: ", paste0("organism = '",uniprot_species_info$name,"', "),".")
      
      uniprot_species_info <-
        uniprot_species_info[1, ]
    }
    
    uniprot_species_info <-
      dplyr::select(
        uniprot_species_info,
        name,
        upid,
        taxonomy,
        isReferenceProteome,
        isRepresentativeProteome,
        superregnum
      )
    
    readr::write_tsv(uniprot_species_info,
                     file.path(tempdir(), paste0(
                       organism_name_path, "_uniprot_info.tsv"
                     )))
    
    
    if (nrow(uniprot_species_info) > 0) {
      #organism_new <- stringr::str_replace_all(uniprot_species_info$name, " ", "%20")
      
      download_file <-
        paste0(uniprot_species_info$upid,
               "_",
               uniprot_species_info$taxonomy,
               ".fasta.gz")
      
      if (uniprot_species_info$superregnum == "arachea") {
        query <-
          paste0(
            "https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes/Archaea/",
            uniprot_species_info$upid,"/",
            download_file
          )
      }
      
      if (uniprot_species_info$superregnum == "bacteria") {
        query <-
          paste0(
            "https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes/Bacteria/",
            uniprot_species_info$upid,"/",
            download_file
          )
      }
      
      if (uniprot_species_info$superregnum == "eukaryota") {
        query <-
          paste0(
            "https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes/Eukaryota/",
            uniprot_species_info$upid,"/",
            download_file
          )
      }
      
      if (uniprot_species_info$superregnum == "viruses") {
        query <-
          paste0(
            "https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/reference_proteomes/Viruses/",
            uniprot_species_info$upid,"/",
            download_file
          )
      }
      message("-> Running download ...")
      if (is.null(path))
        custom_download(url = query, destfile = file.path(tempdir(), download_file))
      
      if (!is.null(path))
        custom_download(url = query, destfile = file.path(path, download_file))
      
      tryCatch({
        organism_fasta_file <-
          Biostrings::readBStringSet(ifelse(is.null(path), file.path(tempdir(), download_file), file.path(path, download_file)))
      }, error = function(e)
        message(
          "Something went wrong when trying to access the Uniprot API .Thus the query ",
          query,
          ". could not be used to retrieve a fasta file for '",
          organism,
          "'. Sometimes the internet connection isn't stable and re-running the function might help. Otherwise, could there be an issue with the firewall?"
        ))
      message("-> Write downloaded *.fasta file to local disk ...")
      Biostrings::writeXStringSet(
        organism_fasta_file,
        filepath = file.path(
          ifelse(is.null(path), getwd(), path),
          paste0(organism_name_path,
                 "_protein_uniprot.faa.gz")
        ),
        compress = TRUE
      )
      
      docFile(
        file.name = paste0(organism_name_path, "_protein_uniprot.faa.gz"),
        organism  = uniprot_species_info$name,
        url       = query,
        database  = "uniprot",
        path      = ifelse(is.null(path), getwd(), path),
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
      
      if (!gunzip) {
        message(
          paste0(
            "The proteome of '",
            organism,
            "' has been downloaded to '",
            ifelse(is.null(path), getwd(), path),
            "' and has been named '",
            paste0(organism_name_path, "_protein_uniprot.faa.gz"),
            "' ."
          )
        )
      }
      
      if (gunzip) {
        message(
          paste0(
            "The proteome of '",
            organism,
            "' has been downloaded to '",
            ifelse(is.null(path), getwd(), path),
            "' and has been named '",
            paste0(organism_name_path, "_protein_uniprot.faa"),
            "' ."
          )
        )
      }
      
      if (gunzip) {
        message("-> Unzipping downloaded file ...")
        R.utils::gunzip(file.path(
          ifelse(is.null(path), getwd(), path),
          paste0(organism_name_path,
                 "_protein_uniprot.faa.gz")
        ),
        destname = file.path(
          ifelse(is.null(path), getwd(), path),
          paste0(organism_name_path,
                 "_protein_uniprot.faa")
        ))
        return(file.path(
          ifelse(is.null(path), getwd(), path),
          paste0(organism_name_path,
                 "_protein_uniprot.faa")
        ))
      } else {
        return(file.path(
          ifelse(is.null(path), getwd(), path),
          paste0(organism_name_path,
                 "_protein_uniprot.faa.gz")
        ))
      }
      
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
