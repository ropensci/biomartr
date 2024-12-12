#' Generic docFile maker
#' @noRd
docFile <-
  function(file.name = doc$file_name,
           organism = doc$organism,
           url = doc$url,
           database = doc$database,
           path = doc$path,
           refseq_category = doc$refseq_category,
           assembly_accession = doc$assembly_accession,
           bioproject = doc$bioproject,
           biosample = doc$bioproject,
           taxid = doc$taxid,
           infraspecific_name = doc$infraspecific_name,
           version_status = doc$version_status,
           release_type = doc$release_type,
           genome_rep = doc$genome_rep,
           seq_rel_date = doc$seq_rel_date,
           submitter = doc$submitter,
           doc = NULL) {
    local.org <- stringr::str_replace_all(organism, "-", "_")
    local.org <- stringr::str_replace_all(organism, "\\/", "_")

    sink(file.path(path, paste0(
      "doc_", local.org, "_db_", database, ".txt"
    )))

    cat(paste0("File Name: ", file.name))
    cat("\n")
    cat(paste0("Organism Name: ", organism))
    cat("\n")
    if (database %in% c("refseq", "genbank"))
      cat(paste0("Database: NCBI ", database))
    if (database %in% c("ensembl", "uniprot"))
      cat(paste0("Database: ", database))
    cat("\n")
    cat(paste0("URL: ", url))
    cat("\n")
    cat(paste0("Download_Date: ", date()))
    cat("\n")
    cat(paste0("refseq_category: ", refseq_category))
    cat("\n")
    cat(paste0("assembly_accession: ", assembly_accession))
    cat("\n")
    cat(paste0("bioproject: ", bioproject))
    cat("\n")
    cat(paste0("biosample: ", biosample))
    cat("\n")
    cat(paste0("taxid: ", taxid))
    cat("\n")
    cat(paste0("infraspecific_name: ", infraspecific_name))
    cat("\n")
    cat(paste0("version_status: ", version_status))
    cat("\n")
    cat(paste0("release_type: ", release_type))
    cat("\n")
    cat(paste0("genome_rep: ", genome_rep))
    cat("\n")
    cat(paste0("seq_rel_date: ", seq_rel_date))
    cat("\n")
    cat(paste0("submitter: ", submitter))

    sink()

  }

docFile_ncbi_metagenome <- function(destfile, organism, download_url, path, metagenomes.members) {
  docFile(
    file.name = destfile,
    organism  = organism,
    url       = download_url,
    database  = "Genbank metagenomes",
    path      = path,
    refseq_category = metagenomes.members$refseq_category,
    assembly_accession = metagenomes.members$assembly_accession,
    bioproject = metagenomes.members$bioproject,
    biosample = metagenomes.members$biosample,
    taxid = metagenomes.members$taxid,
    infraspecific_name = metagenomes.members$infraspecific_name,
    version_status = metagenomes.members$version_status,
    release_type = metagenomes.members$release_type,
    genome_rep = metagenomes.members$genome_rep,
    seq_rel_date = metagenomes.members$seq_rel_date,
    submitter = metagenomes.members$submitter
  )
}
