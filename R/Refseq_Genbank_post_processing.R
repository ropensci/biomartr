refseq_genbank_download_post_processing <- function(info, organism, db, path,
                                                    gunzip,
                                                    remove_annotation_outliers,
                                                    format,
                                                    analyse_genome = FALSE,
                                                    mute_citation = FALSE) {
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
  gunzip_and_check(local_file, gunzip, remove_annotation_outliers, format,
                   mute_citation = mute_citation)
}
