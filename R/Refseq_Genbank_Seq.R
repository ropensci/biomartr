refseqGenbankSeq <- function(db = "refseq",
                             organism,
                             reference = FALSE,
                             skip_bacteria = TRUE,
                             release = NULL,
                             gunzip = FALSE,
                             path = file.path("_ncbi_downloads", "genomes"),
                             assembly_type = "toplevel") {
  # get Kingdom Assembly Summary file
  AssemblyFilesAllKingdoms <-
    getKingdomAssemblySummary(db = db, skip_bacteria = skip_bacteria)

  # test whether or not genome is available
  if (!suppressMessages(is.genome.available(organism = organism, db = db, skip_bacteria = skip_bacteria))) {
    message(
      "Unfortunately no genome file could be found for organism '",
      organism, "'. Thus, the download of this organism has been omitted. Have you tried to specify 'reference = FALSE' ?"
    )
    return("Not available")
  }

  organism_name <- taxid <-
    refseq_category <- version_status <- assembly_accession <- ftp_path <- NULL

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
            assembly_accession ==  organism,
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
            assembly_accession == organism,
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
      "----------> No reference genome or representative genome was found for '",
      organism, "'. Thus, download for this species has been omitted.",
      " Have you tried to specify getGenome(db = '",db,"', organism = '",organism,"' , reference = FALSE) ?",
      " Alternatively, you can retrieve genome assemblies using the NCBI accession ID or NCBI Taxonomy ID.",
      " See '?'is.genome.available' for examples."
    )
    return("Not available")
  }
  if (nrow(FoundOrganism) > 1) {
    message(
      "More than one entry has been found for '",
      organism, "'. Only the first entry '", FoundOrganism$organism_name[1], "' has been used for subsequent genome retrieval.",
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
             "_genomic.fna.gz"
           ))

  local.org <-
    stringr::str_replace_all(organism, "-", "_")
  local.org <-
    stringr::str_replace_all(organism, "\\/", "_")
  local_file <- file.path(path, paste0(local.org, "_genomic_", db, ".fna.gz"))

  custom_download_check_local(download_url, local_file, NULL, "refgen")
  message("-> Genome download of ", organism, " is completed!")

  md5_sum_test(md5_local = file.path(path, paste0(local.org, "_md5checksums.txt")),
               md5_url = paste0(FoundOrganism$ftp_path,"/md5checksums.txt"),
               local_file = local_file, file_url = download_url)

  return(list(local_file = local_file, download_url = download_url,
              local.org = local.org, FoundOrganism = FoundOrganism))
}
