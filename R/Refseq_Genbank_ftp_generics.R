select_assembly_refseq_genbank <- function(organism, AssemblyFilesAllKingdoms,
                                           reference, db) {
  organism_name <- taxid <-
    refseq_category <- version_status <- assembly_accession <- ftp_path <- NULL

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

    is_reference_genome <- FoundOrganism$refseq_category == "reference genome"
    if (any(is_reference_genome)) {
      FoundOrganism <- FoundOrganism[is_reference_genome == TRUE, ]
    }
    is_complete_genome <- FoundOrganism$assembly_level == "Complete Genome"
    if (any(is_complete_genome)) {
      FoundOrganism <- FoundOrganism[is_complete_genome == TRUE, ]
    }

    FoundOrganism <- FoundOrganism[1, ]

    message(
      "More than one entry has been found for '",
      organism, "'. Only the first entry '", FoundOrganism$organism_name[1], "' has been used for subsequent genome retrieval.",
      " If you wish to download a different version, please use the NCBI accession ID when specifying the 'organism' argument.",
      " See ?is.genome.available for examples."
    )
  }
  return(FoundOrganism)
}

ftp_url_refseq_genbank <- function(assembly, type) {
  stem_url <- paste0(assembly$ftp_path, "/", basename(assembly$ftp_path))

  if (type == "genome") {
    url <- paste0(stem_url, "_genomic.fna.gz")
  } else if (type %in% c("gff", "gff3")) {
    url <- paste0(stem_url, "_genomic.gff.gz")
  } else if (type == "gtf") {
    url <- paste0(stem_url, "_genomic.gtf.gz")
  }else if (type == "cds") {
    url <- paste0(stem_url, "_cds_from_genomic.fna.gz")
  } else if (type == "rna") {
    url <- paste0(stem_url, "_rna_from_genomic.fna.gz")
  } else if (type == "protein") {
    url <- paste0(stem_url, "_protein.faa.gz")
  } else if (type == "assembly_stats") {
    url <- paste0(stem_url, "_assembly_stats.txt")
  } else if (type == "repeat_masker") {
    url <- paste0(stem_url, "_rm.out.gz")
  } else stop("invalid refseq/genbank file type selected")

  return(url)
}



local_path_refseq_genbank <- function(path, local.org, db, type) {
  local_file <- file.path(path, local.org)

  if (type == "genome") {
    local_file <- paste0(local_file, "_genomic_", db, ".fna.gz")
  } else if (type %in% c("gff", "gff3")) {
    local_file <- paste0(local_file, "_genomic_", db, ".gff.gz")
  } else if (type == "gtf") {
    local_file <- paste0(local_file, "_genomic_", db, ".gtf.gz")
  }else if (type == "cds") {
    local_file <- paste0(local_file, "_cds_from_genomic_", db, ".fna.gz")
  } else if (type == "rna") {
    local_file <- paste0(local_file, "_rna_from_genomic_", db, ".fna.gz")
  } else if (type == "protein") {
    local_file <- paste0(local_file, "_protein_", db, ".faa.gz")
  } else if (type == "assembly_stats") {
    local_file <- paste0(local_file, "_assembly_stats_", db, ".txt")
  }  else if (type == "repeat_masker") {
    local_file <- paste0(local_file, "_rm_", db, ".out.gz")
  } else stop("invalid refseq/genbank file type selected")

  return(local_file)
}


get_file_refseq_genbank <- function(db = "refseq",
                                    organism,
                                    reference = FALSE,
                                    skip_bacteria = TRUE,
                                    release = NULL,
                                    gunzip = FALSE,
                                    path = file.path("_ncbi_downloads", "genomes"),
                                    assembly_type = "toplevel",
                                    type = "genome") {
  # get Kingdom Assembly Summary file
  AssemblyFilesAllKingdoms <-
    getKingdomAssemblySummary(db = db, skip_bacteria = skip_bacteria)

  # test whether or not genome is available
  if (!suppressMessages(is.genome.available(organism = organism, db = db, skip_bacteria = skip_bacteria))) {
    message(
      "Unfortunately no ",toupper(type), " file could be found for organism '",
      organism, "'. Thus, the download of this organism has been omitted. Have you tried to specify 'reference = FALSE' ?"
    )
    return("Not available")
  }

  organism <- stringr::str_replace_all(organism, "\\(", "")
  organism <- stringr::str_replace_all(organism, "\\)", "")

  FoundOrganism <- select_assembly_refseq_genbank(organism, AssemblyFilesAllKingdoms,
                                                  reference, db)
  download_url <- ftp_url_refseq_genbank(FoundOrganism, type)

  organism <- stringr::str_replace_all(organism, " ", "_")
  local.org <- stringr::str_replace_all(organism, "-", "_")
  local.org <- stringr::str_replace_all(local.org, "\\/", "_")
  local_file <- local_path_refseq_genbank(path, local.org, db, type)

  custom_download_check_local(download_url, local_file, NULL, "refgen")

  if (file.exists(local_file)) {
    message("-> ", toupper(type)," download of ", organism, " is completed!")
    md5_sum_test(md5_local = file.path(path, paste0(local.org, "_md5checksums.txt")),
                 md5_url = paste0(FoundOrganism$ftp_path,"/md5checksums.txt"),
                 local_file = local_file, file_url = download_url)
  } else local_file <- "Not available"


  return(list(local_file = local_file, download_url = download_url,
              local.org = local.org, FoundOrganism = FoundOrganism))
}
