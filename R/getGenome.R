#' @inherit getBio
#' @title Genome Retrieval
#' @description  Main genome retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest the
#' corresponding fasta-file storing the genome of the organism of interest
#' can be downloaded and stored locally. Genome files can be retrieved from
#' several databases. In addition, the genome summary statistics for the
#' retrieved species is stored locally to provide users with
#' insights regarding the genome assembly quality (see \code{\link{summary_genome}} for details).
#' This is useful when comparing genomes with large difference in genome assembly qualities.
#' @family getBio
#' @family genome
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
      info <- get_file_refseq_genbank(db, organism, reference, skip_bacteria,
                                      release, gunzip, path, assembly_type,
                                      type = "genome")
      refseq_genbank_download_post_processing(info, organism, db, path,
                                              gunzip,
                                              remove_annotation_outliers = FALSE,
                                              format = "genome",
                                              analyse_genome = analyse_genome,
                                              mute_citation = mute_citation)
    } else if (db %in% c("ensembl", "ensemblgenomes")) {
        genome.path <- getENSEMBL.Seq(organism, type = "dna",
                                      id.type = assembly_type,
                                      release = release,
                                      path = path)

        ensembl_download_post_processing(genome.path, organism,
                                         format = "genome",
                                         gunzip = gunzip,
                                         mute_citation = mute_citation)
    }
}





