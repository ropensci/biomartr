#' @inherit getBioSet
#' @title CDS retrieval of multiple species
#' @description Main CDS retrieval function for a set of organism of interest.
#' By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the CDS of the organisms of interest
#' will be downloaded and stored locally. CDS files can be retrieved from several databases.
#' @export
getCDSSet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           gunzip = TRUE,
           update = FALSE,
           path = "set_CDS") {
    getBioSet(db, organisms, set_type = "cds", reference,
              release = release, gunzip = gunzip, update = update,
              path = path)
}

#' @inherit getBioSet
#' @title Proteome retrieval of multiple species
#' @description Main proteome retrieval function for a set of organism of interest.
#' By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the proteome of the organisms of interest
#' will be downloaded and stored locally. proteome files can be retrieved from several databases.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding proteomes shall be stored. Default is \code{path} = \code{"set_proteomes"}.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#' # download the proteomes of three different species at the same time
#' #### Database: NCBI RefSeq
#' file_paths <- getProteomeSet(db = "refseq", organisms = c("Arabidopsis thaliana",
#'                                       "Arabidopsis lyrata",
#'                                        "Capsella rubella"))
#' # look at file paths
#' file_paths
#'
#' #### Database: NCBI Genbank
#' file_paths <- getProteomeSet(db = "genbank", organisms = c("Arabidopsis thaliana",
#'                                       "Arabidopsis lyrata",
#'                                        "Capsella rubella"))
#' # look at file paths
#' file_paths
#'
#' # download the proteomes of three different species at the same time
#' #### Database: ENSEMBL
#' file_paths <- getProteomeSet(db = "ensembl", organisms = c("Homo sapiens",
#'                                       "Mus musculus",
#'                                        "Caenorhabditis elegans"))
#' # look at file paths
#' file_paths
#'
#' # download the proteomes of three different species at the same time
#' #### Database: UniProt
#' file_paths <- getProteomeSet(db = "uniprot", organisms = c("Homo sapiens",
#'                                       "Mus musculus",
#'                                        "Caenorhabditis elegans"))
#' # look at file paths
#' file_paths
#' }
#'
#' @seealso \code{\link{read_proteome}}
#' @export
getProteomeSet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           skip_bacteria = TRUE,
           gunzip = TRUE,
           update = FALSE,
           path = "set_proteomes",
           mute_citation = FALSE) {
    getBioSet(db,
              organisms,
              set_type = "proteome",
              reference = reference,
              release = release,
              skip_bacteria = skip_bacteria,
              gunzip = gunzip,
              update = update,
              path = path,
              mute_citation = mute_citation)
  }

#' @inherit getBioSet
#' @title GFF retrieval of multiple species
#' @description Main GFF retrieval function for a set of organism of interest.
#' By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the GFF of the organisms of interest
#' will be downloaded and stored locally. GFF files can be retrieved from several databases.
#' @export
getGFFSet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           gunzip = TRUE,
           remove_annotation_outliers = FALSE,
           update = FALSE,
           format = "gff3",
           path = "set_GFF",
           mute_citation = FALSE) {
    getBioSet(db,
              organisms,
              set_type = "gff",
              reference = reference,
              release = release,
              skip_bacteria = skip_bacteria,
              gunzip = gunzip,
              remove_annotation_outliers = remove_annotation_outliers,
              update = update,
              format = format,
              path = path,
              mute_citation = mute_citation)
  }

#' @inherit getBioSet
#' @title Genome Retrieval of multiple species
#' @description Main genome retrieval function for a set of organism of interest.
#' By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the genome of the organisms of interest
#' will be downloaded and stored locally. Genome files can be retrieved from several databases.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding genomes shall be stored. Default is
#' \code{path} = \code{"set_genomes"}.
#' @export
getGenomeSet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           skip_bacteria = TRUE,
           gunzip = TRUE,
           update = FALSE,
           path = "set_genomes",
           assembly_type = "toplevel",
           mute_citation = FALSE) {
    getBioSet(db,
              organisms,
              set_type = "genome",
              reference = reference,
              release = release,
              skip_bacteria = skip_bacteria,
              gunzip = gunzip,
              update = update,
              path = path,
              assembly_type = assembly_type,
              mute_citation = mute_citation)

  }

#' @inherit getBioSet
#' @title RNA Retrieval of multiple species
#' @description Main RNA retrieval function for a set of organism of interest.
#' By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the RNA of the organisms of interest
#' will be downloaded and stored locally. RNA files can be retrieved from several databases.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding RNAs shall be stored. Default is \code{path} = \code{"set_RNAs"}.
#' @export
getRNASet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           skip_bacteria = TRUE,
           gunzip = TRUE,
           update = FALSE,
           path = "set_RNAs",
           mute_citation = FALSE) {
    getBioSet(db,
              organisms,
              set_type = "genome",
              reference = reference,
              release = release,
              skip_bacteria = skip_bacteria,
              gunzip = gunzip,
              update = update,
              path = path,
              mute_citation = mute_citation)
  }

#' @inherit getBioSet
#' @title Retrieve a Collection: Genome, Proteome, CDS, RNA, GFF, Repeat Masker, AssemblyStats of multiple species
#' @description Main collection retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest a collection consisting of
#' the genome file, proteome file, CDS file, RNA file, GFF file, Repeat Masker file, AssemblyStats
#' file of the organism of interest
#' can be downloaded and stored locally. Collections can be retrieved from
#' several databases.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding collection shall be stored. Default is
#' \code{path} = \code{file.path("_db_downloads","collections")}.
#' @export
getCollectionSet <-
  function(db = "refseq",
           organisms,
           reference = FALSE,
           release = NULL,
           skip_bacteria = TRUE,
           gunzip = TRUE,
           update = FALSE,
           remove_annotation_outliers = TRUE,
           path = "set_collections",
           mute_citation = FALSE) {
    getBioSet(db,
              organisms,
              set_type = "collection",
              reference = reference,
              release = release,
              skip_bacteria = skip_bacteria,
              gunzip = gunzip,
              update = update,
              remove_annotation_outliers = remove_annotation_outliers,
              path = path,
              mute_citation = mute_citation)
}
