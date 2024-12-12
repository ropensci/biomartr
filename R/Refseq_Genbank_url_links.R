
refseq_genbank_ftp_server_url <- function() {
  return("https://ftp.ncbi.nlm.nih.gov/")
}

refseq_genbank_ftp_server_url_genome <- function() {
  return(paste0(refseq_genbank_ftp_server_url(), "genomes/"))
}


refseq_genbank_ftp_server_url_genome_specific <- function(id = "refseq") {
  stopifnot(id %in% c("refseq", "genbank"))
  url <- refseq_genbank_ftp_server_url_genome()
  url <- paste0(url, id, "/")
  return(url)
}

refseq_kingdoms_url <- function(kingdoms = getKingdomsNCBI("refseq"), all_refseq_kingdoms_vector = getKingdomsNCBI("refseq")) {
  refseq_genbank_ftp_server_kingdoms_url(kingdoms, "refseq", all_refseq_kingdoms_vector)
}

refseq_genbank_ftp_server_kingdoms_url <- function(kingdoms, db = "refseq") {
  all_kingdoms_vector <- getKingdomsNCBI(db)
  if (!all(kingdoms %in% all_kingdoms_vector)) {
    stop("Valid kingdoms for db: '", db, "' are: \n",
         paste(all_kingdoms_vector, collapse = ", "))
  }
  return(paste0(refseq_genbank_ftp_server_url_genome_specific(), kingdoms, "/"))
}

refseq_genbank_genome_report_url <- function() {
  return(paste0(refseq_genbank_ftp_server_url_genome(), "GENOME_REPORTS", "/overview.txt"))
}

refseq_genbank_blast_ftp_server_url_blast <- function() {
  return(paste0(refseq_genbank_ftp_server_url(), "blast/db/"))
}

refseq_genbank_blast_ftp_server_url_blast_db <- function(db) {
  return(paste0(refseq_genbank_blast_ftp_server_url_blast(), db))
}

refseq_genbank_blast_ftp_server_url_blast_db_md5 <- function(db) {
  return(paste0(refseq_genbank_blast_ftp_server_url_blast_db(db), ".md5"))
}

refseq_genbank_ftp_server_url_metagenome_specific <- function(id = "genbank") {
  return(paste0(refseq_genbank_ftp_server_url_genome_specific(id), "metagenomes/assembly_summary.txt"))
}

refseq_genbank_ftp_server_url_genome_specific_summary <- function(kingdom, db) {
  return(paste0(refseq_genbank_ftp_server_kingdoms_url(kingdom, db), "/assembly_summary.txt"))
}


