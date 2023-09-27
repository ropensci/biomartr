
refseqGenbankAnnotation <- function(db = "refseq",
                                    organism,
                                    reference = FALSE,
                                    skip_bacteria = TRUE,
                                    path = file.path("_ncbi_downloads", "annotation"),
                                    format = "gff3") {
  # get Kingdom Assembly Summary file
  AssemblyFilesAllKingdoms <-
    getKingdomAssemblySummary(db = db, skip_bacteria = skip_bacteria)

  # test whether or not genome is available
  suppressMessages(is.genome.available(organism = organism, db = db, skip_bacteria = skip_bacteria))



  organism_name <- assembly_accession <- taxid <-
    refseq_category <- version_status <- ftp_path <- NULL

  organism <-
    stringr::str_replace_all(organism, "\\(", "")
  organism <-
    stringr::str_replace_all(organism, "\\)", "")


  if (reference) {
    if (!is.taxid(organism)) {
      FoundOrganism <-
        dplyr::filter(
          AssemblyFilesAllKingdoms,
          stringr::str_detect(organism_name, organism) |
            stringr::str_detect(assembly_accession, organism),
          ((refseq_category == "representative genome") |
             (refseq_category == "reference genome")
          ),
          (version_status == "latest"), !is.na(ftp_path)
        )
    } else {
      FoundOrganism <-
        dplyr::filter(
          AssemblyFilesAllKingdoms,
          taxid == as.integer(organism),
          ((refseq_category == "representative genome") |
             (refseq_category == "reference genome")
          ),
          (version_status == "latest"), !is.na(ftp_path))
    }
  } else {
    if (!is.taxid(organism)) {
      FoundOrganism <-
        dplyr::filter(
          AssemblyFilesAllKingdoms,
          stringr::str_detect(organism_name, organism) |
            stringr::str_detect(assembly_accession, organism),
          (version_status == "latest"), !is.na(ftp_path)
        )
    } else {
      FoundOrganism <-
        dplyr::filter(
          AssemblyFilesAllKingdoms,
          taxid == as.integer(organism),
          (version_status == "latest"), !is.na(ftp_path)
        )
    }
  }

  if (nrow(FoundOrganism) == 0) {
    message(
      paste0(
        "----------> No reference GFF file was found for '",
        organism, "'. Thus, download for this organism has been omitted.",
        " Have you tried to specify getGFF(db = '",db,"', organism = '",organism,"' , reference = FALSE) ?",
        " Alternatively, you can retrieve GFF files using the NCBI accession ID or NCBI Taxonomy ID.",
        " See '?'is.genome.available' for examples."
      )
    )
    return(FALSE)
  } else {
    if (nrow(FoundOrganism) > 1) {
      warnings(
        "More than one entry has been found for '",
        organism, "'. Only the first entry '", FoundOrganism[1, 1], "' has been used for subsequent GFF retrieval.",
        " If you wish to download a different version, please use the NCBI accession ID when specifying the 'organism' argument.",
        " See ?is.genome.available for examples."
      )
      FoundOrganism <- FoundOrganism[1, ]
  }

  organism <-
    stringr::str_replace_all(organism, " ", "_")

  download_url <-
    paste0(FoundOrganism$ftp_path,
           "/",
           paste0(
             basename(FoundOrganism$ftp_path),
             "_genomic.gff.gz"
           ))

  local.org <-
    stringr::str_replace_all(organism, "-", "_")
  local.org <-
    stringr::str_replace_all(organism, "\\/", "_")
  local_file <- file.path(path, paste0(local.org, "_genomic_", db, ".gff.gz"))

  custom_download_check_local(download_url, local_file, NULL, "refgen")

  message("-> GFF download of ", organism, " is completed!")

  md5_sum_test(md5_local = file.path(path, paste0(local.org, "_md5checksums.txt")),
               md5_url = paste0(FoundOrganism$ftp_path,"/md5checksums.txt"),
               local_file = local_file, file_url = download_url)
  }
  return(list(local_file = local_file, download_url = download_url,
              local.org = local.org, FoundOrganism = FoundOrganism))
}

refseq_genbank_download_post_processing <- function(info, organism, db, path,
                                                    gunzip,
                                                    remove_annotation_outliers,
                                                    format,
                                                    analyse_genome = FALSE) {
  if ((is.logical(info[1]) && !info) || (info[1] == "Not available"))
    return("Not available")

  local.org <- info$local.org
  local_file <- info$local_file
  download_url <- info$download_url
  FoundOrganism <- info$FoundOrganism
  # Save Annotation information
  doc <- tibble::tibble(
    file_name = local_file,
    organism  = organism,
    url       = download_url,
    database  = db,
    path      = path,
    refseq_category = FoundOrganism$refseq_category,
    assembly_accession = FoundOrganism$assembly_accession,
    bioproject = FoundOrganism$bioproject,
    biosample = FoundOrganism$biosample,
    taxid = FoundOrganism$taxid,
    infraspecific_name = FoundOrganism$infraspecific_name,
    version_status = FoundOrganism$version_status,
    release_type = FoundOrganism$release_type,
    genome_rep = FoundOrganism$genome_rep,
    seq_rel_date = FoundOrganism$seq_rel_date,
    submitter = FoundOrganism$submitter
  )
  docFile(doc = doc)

  if (analyse_genome) {
    genome_summary_stats <- summary_genome(file = local_file, organism = organism)
    genome_analysis_path <- file.path(path, paste0("doc_",local.org,"_db_",db,"_summary_statistics.tsv"))
    readr::write_tsv(genome_summary_stats, file = genome_analysis_path)
  }


  doc_file_path <- file.path(path,
                             paste0("doc_", local.org, "_db_", db, ".tsv"))
  readr::write_tsv(doc, file = doc_file_path)
  gunzip_and_check(local_file, gunzip, remove_annotation_outliers, format)
}
