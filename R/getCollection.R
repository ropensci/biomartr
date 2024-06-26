#' @inherit getBio
#' @title Retrieve a Collection: Genome, Proteome, CDS, RNA, GFF, Repeat Masker, AssemblyStats
#' @description  Main collection retrieval function for an organism of interest.
#' By specifying the scientific name of an organism of interest a collection consisting of
#' the genome file, proteome file, CDS file, RNA file, GFF file, Repeat Masker file, AssemblyStats
#' file of the organism of interest
#' can be downloaded and stored locally. Collections can be retrieved from
#' several databases. For full set of collection elements, see:
#' biomartr:::supported_biotypes(db)
#' @param path a character string specifying the location (a folder) in which
#' the corresponding collection shall be stored. Default is
#' \code{path} = \code{file.path("_db_downloads","collections")}.
#' @examples \dontrun{
#' # download the collection of Homo sapiens from refseq
#' # and store the corresponding genome file in '_ncbi_downloads/collection'
#'  Hsap_collection <- getCollection( db       = "refseq",
#'              organism = "Homo sapiens",
#'              path = file.path("_db_downloads","collections"))
#' # download the collection of Homo sapiens from genbank
#' # and store the corresponding genome file in '_ncbi_downloads/collection'
#'  Hsap_collection <- getCollection( db       = "genbank",
#'              organism = "Homo sapiens",
#'              path = file.path("_db_downloads","collections"))
#' # download the collection of Homo sapiens from ensembl
#' # and store the corresponding genome file in '_ncbi_downloads/collection'
#'  Hsap_collection <- getCollection( db       = "ensembl",
#'              organism = "Homo sapiens",
#'              path = file.path("_db_downloads","collections"))
#' }
#'
#' @family getBio
#' @family collection
#' @export
getCollection <-
        function(db = "refseq",
                 organism,
                 reference = TRUE,
                 skip_bacteria = TRUE,
                 release = NULL,
                 assembly_type  = "toplevel",
                 analyse_genome = FALSE,
                 remove_annotation_outliers = FALSE,
                 gunzip = FALSE,
                 path = file.path("_db_downloads","collections"),
                 mute_citation = FALSE
        ) {

    new_name <- stringr::str_replace_all(organism," ","_")
    all_biotypes <- supported_biotypes(db)
    message("-> Starting collection retrieval (", paste(all_biotypes, collapse = ", "),") for ", new_name, " ...")

    org_exists <- is.genome.available(db, organism, skip_bacteria, details = TRUE)

    if (isFALSE(org_exists) || length(org_exists) == 0)
        stop("-> No entry was found for organism ",organism,". Could the name be misspelled?",  call. = FALSE)

    path <- file.path(path, db, new_name)
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)



    for (type in all_biotypes) {
      getBio(db, organism, type,
             reference = reference, release = release, gunzip = gunzip,
             update = FALSE, skip_bacteria = skip_bacteria,
             path = path,
             remove_annotation_outliers = remove_annotation_outliers,
             analyse_genome = analyse_genome, assembly_type = assembly_type,
             format = names(all_biotypes[all_biotypes == type]), mute_citation = TRUE)
      message("\n")
    }

    output_files <- list.files(path)
    # Remove md5 files
    md5_files <- output_files[stringr::str_detect(output_files, "md5checksums.txt")]
    file.remove(file.path(path, md5_files))

    # Move doc files
    doc_folder <- file.path(path, "doc")
    if (!file.exists(doc_folder)) {
      dir.create(doc_folder)
    }
    doc_files <- output_files[stringr::str_detect(output_files, "doc_")]
    file.rename(file.path(path, doc_files),
                file.path(doc_folder, doc_files))
    # Verify assembly doc is now valid
    readAssemblyDoc(doc_folder, db)

    message("-> Collection retrieval finished successfully!")
    message("\n")
    please_cite_biomartr(mute_citation = mute_citation)
    return(path)
}







