#' @title Genome Assembly Stats Retrieval
#' @description  Main genome assembly stats retrieval function for an organism
#' of interest. By specifying the scientific name of an organism of interest the
#' corresponding  genome assembly stats file storing the assembly statistics of
#' the organism of interest can be downloaded and stored locally.
#' Genome assembly stats files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param organism a character string specifying the scientific name of the
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param type shall only the file be retrieved (default)
#' \code{type = "download"} or should the corresponding file be downloaded and
#' subsequently be imported \code{type = "import"}.
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @param path a character string specifying the location (a folder) in
#' which the corresponding file shall be stored. Default is
#' \code{path} = \code{file.path("_ncbi_downloads","genomeassembly_stats")}.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' to retrieve available scientific names of organisms and creates a directory
#' '_ncbi_downloads/genomeassembly_stats' to store
#' the Genome Assembly Stats of interest as text file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/genomeassembly_stats' folder and is
#' accessible within the workspace, no download process will be performed.
#'
#' An example genome assembly stats file can be found here:
#' ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/001/405/
#' GCF_000001405.36_GRCh38.p10/GCF_000001405.36_GRCh38.p10_assembly_stats.txt.
#'
#' @return File path to downloaded genome assembly stats file.
#' @examples \dontrun{
#' # download the genome assembly stats file of Saccharomyces cerevisiae
#' # from NCBI RefSeq
#' # and store the corresponding genome file in
#' # '_ncbi_downloads/genomeassembly_stats'
#' file_path <- getAssemblyStats( db = "refseq",
#'                  organism = "Saccharomyces cerevisiae",
#'                  path = file.path("_ncbi_downloads","genomeassembly_stats"))
#' # import the raw file as it is downloaded
#' Scerevisiae.stats <- read_assemblystats(file_path, type = "raw")
#'
#' # download the genome assembly stats file of Saccharomyces cerevisiae
#' # from NCBI RefSeq
#' # and import overall statistics of the genome assembly
#' Scerevisiae.stats.import <- getAssemblyStats( db = "refseq",
#'                  organism = "Saccharomyces cerevisiae",
#'                  type = "import",
#'                  path = file.path("_ncbi_downloads","genomeassembly_stats"))
#' }
#'
#' @seealso \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{getCollection}}, \code{\link{meta.retrieval}},
#' \code{\link{read_assemblystats}}
#' @export

getAssemblyStats <-
  function(db = "refseq",
           organism,
           reference = FALSE,
           skip_bacteria = TRUE,
           release = NULL,
           type = "download",
           path = file.path("_ncbi_downloads", "genomeassembly_stats"),
           mute_citation = FALSE) {
    if (!is.element(db, c("refseq", "genbank")))
      stop(
        paste0(
          "Please select one of the available data bases: ",
          "'refseq' and 'genbank'."
        ),
        call. = FALSE
      )

    if (!is.element(type, c("download", "import")))
      stop(
        paste0(
          "Please choose either type = 'download' ",
          "(if you would like to ",
          "download the genome assembly stats file) or ",
          "type = 'import' (if you would like to download and import the ",
          "genome assembly stats file).",
          collapse = ""
        ),
        call. = FALSE
      )

    info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                    release, gunzip, path, type = "assembly_stats")
    assembly_file <-
    refseq_genbank_download_post_processing(info, organism, db, path,
                                            gunzip = FALSE,
                                            remove_annotation_outliers = FALSE,
                                            format = "assembly_stats",
                                            mute_citation = mute_citation)
    if (type == "import" & assembly_file != "Not available") {
      assembly_stats_file <- read_assemblystats(assembly_file, type = "stats")
      assembly_file <- dplyr::bind_cols(tibble::tibble(species = organism), assembly_stats_file)
    }
    return(assembly_file)
  }
