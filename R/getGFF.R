#' @title Genome Annotation Retrieval (GFF3)
#' @description  Main retrieval function for GFF files of an
#' organism of interest. By specifying the scientific name of an organism of
#' interest the corresponding gff file storing the annotation  for the organism
#' of interest can be downloaded and stored locally. GFF files can be retrieved
#' from several databases.
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param organism a character string specifying the scientific name of the
#' organism of interest, e.g. \code{organism = "Homo sapiens"}.
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @param release the database release version of ENSEMBL (\code{db = "ensembl"}). Default is \code{release = NULL} meaning
#' that the most recent database version is used.
#' @param gunzip a logical value indicating whether or not files should be unzipped.
#' @param remove_annotation_outliers shall outlier lines be removed from the input \code{annotation_file}?
#' If yes, then the initial \code{annotation_file} will be overwritten and the removed outlier lines will be stored at \code{\link{tempdir}}
#' for further exploration.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding annotation file shall be stored. Default is
#' \code{path = file.path("_ncbi_downloads","genomes")}.
#' @param format "gff3", alternative "gtf" for ensembl.
#' @param mute_citation logical value indicating whether citation message should be muted.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory '_ncbi_downloads/annotation' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/annotation' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded annotation file.
#' @examples \dontrun{
#' # download the annotation of Arabidopsis thaliana from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/annotation'
#' Athal_gff <- getGFF( db       = "refseq",
#'                organism = "Arabidopsis thaliana",
#'                path = file.path("_ncbi_downloads","annotation"),
#'                remove_annotation_outliers = TRUE)
#' Athal_gff_import <- read_gff(Athal_gff)
#'
#'
#' # download the genome of Arabidopsis thaliana from genbank
#' # and store the corresponding genome file in '_ncbi_downloads/annotation'
#' Athal_gff <- getGFF( db       = "genbank",
#'                organism = "Arabidopsis thaliana",
#'                path = file.path("_ncbi_downloads","annotation"),
#'                remove_annotation_outliers = TRUE)
#' Athal_gff_import <- read_gff(Athal_gff)
#'
#' # download the genome of Homo sapiens from ensembl
#' # and store the corresponding genome file in '_ncbi_downloads/annotation'
#' Hsap_gff <- getGFF( db       = "ensembl",
#'                organism = "Homo sapiens",
#'                path = file.path("_ncbi_downloads","annotation"),
#'                remove_annotation_outliers = TRUE)
#' Hsap_gff_import <- read_gff(Hsap_gff)
#'
#' }
#'
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGenome}}, \code{\link{getRNA}}, \code{\link{getRepeatMasker}},
#' \code{\link{getAssemblyStats}}, \code{\link{getCollection}}, \code{\link{meta.retrieval}}
#' @export

getGFF <- function(db = "refseq", organism, reference = FALSE,
                   skip_bacteria = TRUE, release = NULL, gunzip = FALSE,
                   remove_annotation_outliers = FALSE,
                   path = file.path("_ncbi_downloads", "annotation"),
                   mute_citation = FALSE, format = "gff3") {

   if (!is.element(db, c("refseq", "genbank", "ensembl")))
        stop(
            "Please select one of the available data bases: 'refseq',
            'genbank', or 'ensembl'."
        )
    message("-> Starting ", toupper(format)," retrieval of '", organism, "' from ", db, " ...")
    message("\n")

    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    if (is.element(db, c("refseq", "genbank"))) {
      info <- refseqGenbankAnnotation(db, organism, reference, skip_bacteria,
                              path, format)
      return(refseq_genbank_download_post_processing(info, organism, db, path,
                                                     gunzip,
                                                     remove_annotation_outliers,
                                                     format,
                                                     mute_citation = mute_citation))
    }

    if (db == "ensembl") {
        genome.path <- getENSEMBL.Annotation(organism, type = "dna",
                                             release = release, path = path,
                                             format = format)
        return(ensembl_download_post_processing(genome.path, organism, format,
                                                remove_annotation_outliers,
                                                gunzip,
                                                mute_citation = mute_citation))
    }
}

#' @title Genome Annotation Retrieval (GTF)
#' @description  Main retrieval function for GTF files of an
#' organism of interest. By specifying the scientific name of an organism of
#' interest the corresponding GTF file storing the annotation  for the organism
#' of interest can be downloaded and stored locally. GTF files can be retrieved
#' from several databases.
#' @inheritParams getGFF
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "ensembl"}
#' }
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from ENSEMBL:
#' and creates a directory 'ensembl/annotation' to store
#' the genome of interest as fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' 'ensembl/annotation' folder and is accessible within the workspace,
#' no download process will be performed.
#' @inheritParams getENSEMBL.Seq
#' @return File path to downloaded annotation file.
#' @examples \dontrun{
#' # download the annotation of Homo sapiens from ensembl
#' # and store the corresponding genome file in 'ensembl/annotation'
#' getGTF(db            = "ensembl",
#'        organism      = "Homo sapiens",
#'        path          = file.path("ensembl","annotation"))
#'
#' getGTF(db            = "ensembl",
#'        organism      = "Homo sapiens",
#'        path          = file.path("ensembl","annotation"),
#'        assembly_type = "primary_assembly")
#'
#' }
#'
#' @seealso \code{\link{getProteome}}, \code{\link{getCDS}},
#' \code{\link{getGenome}}, \code{\link{getRNA}}, \code{\link{getRepeatMasker}},
#' \code{\link{getAssemblyStats}}, \code{\link{meta.retrieval}},
#' \code{\link{getGFF}}
#' @export
getGTF <-
  function(db = "ensembl",
           organism,
           remove_annotation_outliers = FALSE,
           path = file.path("ensembl", "annotation"),
           release = NULL,
           mute_citation = FALSE) {
    if (!is.element(db, c("ensembl")))
      stop( "Please select one of the available data bases: db = 'ensembl'.", call. = FALSE)
    getGFF(db = "ensembl",
           organism,
           remove_annotation_outliers = remove_annotation_outliers,
           path = path,
           release = release,
           mute_citation = mute_citation,
           format = "gtf")
  }

