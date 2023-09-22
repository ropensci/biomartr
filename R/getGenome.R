#' @title Genome Retrieval
#' @description  Main genome retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest the
#' corresponding fasta-file storing the genome of the organism of interest
#' can be downloaded and stored locally. Genome files can be retrieved from
#' several databases. In addition, the genome summary statistics for the
#' retrieved species is stored locally to provide users with
#' insights regarding the genome assembly quality (see \code{\link{summary_genome}} for details).
#' This is useful when comparing genomes with large difference in genome assembly qualities.
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param organism there are three options to characterize an organism:
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @inheritParams getENSEMBL.Seq
#' @param gunzip a logical value indicating whether or not files should be unzipped.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding genome shall be stored. Default is
#' \code{path} = \code{file.path("_ncbi_downloads","genomes")}.
#' @inheritParams getGTF
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory '_ncbi_downloads/genomes' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomes' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded genome.
#' @examples \dontrun{
#'
#' # download the genome of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' file_path <- getGenome( db       = "refseq",
#'              organism = "Arabidopsis thaliana",
#'              path = file.path("_ncbi_downloads","genomes"))
#'
#' Ath_genome <- read_genome(file_path, format = "fasta")
#'
#'
#' # download the genome of Arabidopsis thaliana from genbank
#' # and store the corresponding genome file in '_ncbi_downloads/genomes'
#' file_path <- getGenome( db       = "genbank",
#'              organism = "Arabidopsis thaliana",
#'              path = file.path("_ncbi_downloads","genomes"))
#'
#' Ath_genome <- read_genome(file_path, format = "fasta")
#' }
#'
#' @seealso \code{\link{getGenomeSet}}, \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{getRepeatMasker}},
#' \code{\link{getAssemblyStats}}, \code{\link{summary_genome}}, \code{\link{getCollection}},
#' \code{\link{meta.retrieval}}, \code{\link{meta.retrieval.all}}, \code{\link{read_genome}}
#' @export

getGenome <- function(db = "refseq",
             organism,
             reference = FALSE,
             skip_bacteria = TRUE,
             release = NULL,
             gunzip = FALSE,
             path = file.path("_ncbi_downloads", "genomes"),
             assembly_type = "toplevel",
             mute_citation = FALSE,
             analyse_genome = FALSE
             ) {

       if (!is.element(db, c("refseq", "genbank", "ensembl")))
            stop(
                "Please select one of the available data bases: 'refseq',
                'genbank', or 'ensembl'.",
                call. = FALSE
            )
        if (!(assembly_type %in% c("toplevel", "primary_assembly")))
            stop("Please select one the available assembly types: \ntoplevel, primary_assembly")
        if ((db != "ensembl") && (assembly_type != "toplevel"))
            stop( "The assembly_type argument is not default value.",
            "Don't change this argument when not using db = 'ensembl'.", call. = FALSE)

        if (!is.logical(reference))
            stop("Please specify 'reference' as either TRUE or FALSE.", call. = FALSE)


      message("-> Starting genome retrieval of '", organism, "' from ", db, " ...")
      message("\n")

      # create result folder
      if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
      }

    if (is.element(db, c("refseq", "genbank"))) {
      info <- refseqGenbankSeq(db, organism, reference, skip_bacteria,
                               release, gunzip,
                               path, assembly_type)
      return(refseq_genbank_download_post_processing(info, organism, db, path,
                                                     gunzip,
                                                     remove_annotation_outliers,
                                                     format = "genome"))
    }

    if (db %in% c("ensembl", "ensemblgenomes")) {
        genome.path <- getENSEMBL.Seq(organism, type = "dna",
                                      id.type = assembly_type,
                                      release = release,
                                      path = path)
        ensembl_download_post_processing(genome.path, organism,
                                         format = "genome",
                                         gunzip = gunzip)
    }
}





