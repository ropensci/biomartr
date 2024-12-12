#' @title Retrieve annotation *.gff files for metagenomes from NCBI Genbank
#' @description Retrieve available annotation *.gff files for metagenomes
#' from NCBI Genbank. NCBI Genbank allows users
#' to download entire metagenomes and their annotations of several metagenome
#' projects. This function downloads available metagenomes that can then be
#' downloaded via \code{\link{getMetaGenomes}}.
#' @inheritParams getMetaGenomes
#' @param path a character string specifying the location (a folder)
#' in which the corresponding metagenome annotations shall be stored.
#' Default is
#' \code{path} = \code{file.path("_ncbi_downloads","metagenome","annotations")}.
#' @param metagenomes.members a tibble with selected assemblies,
#' default: dplyr::filter(getMetaGenomeSummary(), organism_name == name & total_gene_count > 0)).
#' This is different to getMetaGenome since it requires a gff to exist, most genbank
#' assemblies are .gbff files only, which are usually not useful.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # Frist, retrieve a list of available metagenomes
#' listMetaGenomes()
#'
#' # Now, retrieve the 'human gut metagenome'
#' getMetaGenomeAnnotations(name = "human gut metagenome")
#' }
#' @seealso \code{\link{getMetaGenomes}}, \code{\link{listMetaGenomes}},
#' \code{\link{getGFF}}
#' @export
getMetaGenomeAnnotations <- function(name,
             path = file.path("_ncbi_downloads", "metagenome", "annotations"),
             metagenomes.members = dplyr::filter(getMetaGenomeSummary(), organism_name == name & total_gene_count > 0)) {
    if (!is.element(name, listMetaGenomes(details = FALSE)))
        stop(
            paste0("Unfortunately, the metagenome '",
            name,
            "' is not available. Please consult the listMetaGenomes() ",
            "function for available metagenomes.")
        )

    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    if (!is(metagenomes.members, "tbl_df") | nrow(metagenomes.members) == 0) {
      stop("Argument 'metagenomes.members' must be a tibble of nrow > 0")
    }

    file.names <- metagenomes.members$ftp_path
    destfiles <- file.path(path, paste0(
      basename(file.names), "_genomic.gff.gz"))
    download_urls <- paste0(
      file.names,
      "/",
      paste0(
        metagenomes.members$assembly_accession,
        "_",
        metagenomes.members$asm_name,
        "_genomic.gff.gz"
      )
    )
    for (i in seq_len(length(file.names))) {
      custom_download_check_local(download_urls[i], destfiles[i], NULL, db = "genbank")
      docFile_ncbi_metagenome(basename(destfiles[i]), basename(file.names[i]),
                              download_urls[i], path, metagenomes.members[i,])
    }
    message("The annotations of metagenome '", name, "' has been downloaded to '",
            path, "'.")

    file.paths <- file.path(path, list.files(path = path))
    # return only file paths without "*.txt"
    return(file.paths[!unlist(lapply(file.paths, function(x)
        stringr::str_detect(x, "[.]txt")))])
}
