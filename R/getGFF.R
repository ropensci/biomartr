#' @inherit getBio
#' @title Genome Annotation Retrieval (GFF3)
#' @description  Main retrieval function for GFF files of an
#' organism of interest. By specifying the scientific name of an organism of
#' interest the corresponding gff file storing the annotation  for the organism
#' of interest can be downloaded and stored locally. GFF files can be retrieved
#' from several databases.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding annotation file shall be stored. Default is
#' \code{path = file.path("_ncbi_downloads","annotation")}.
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
#' @family getBio
#' @family gff
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
      info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                      release, gunzip, path, type = "gff")
      return(refseq_genbank_download_post_processing(info, organism, db, path,
                                                     gunzip,
                                                     remove_annotation_outliers,
                                                     format,
                                                     mute_citation = mute_citation))
    } else if (db == "ensembl") {
        genome.path <- getENSEMBL.Annotation(organism, type = "dna",
                                             release = release, path = path,
                                             format = format)
        return(ensembl_download_post_processing(genome.path, organism, format,
                                                remove_annotation_outliers,
                                                gunzip,
                                                mute_citation = mute_citation))
    }
}

#' @inherit getGFF
#' @title Genome Annotation Retrieval (GTF)
#' @description  Main retrieval function for GTF files of an
#' organism of interest. By specifying the scientific name of an organism of
#' interest the corresponding GTF file storing the annotation  for the organism
#' of interest can be downloaded and stored locally. GTF files can be retrieved
#' from several databases.
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

