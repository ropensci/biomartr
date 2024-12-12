#' @title Retrieve metagenomes from NCBI Genbank
#' @description Retrieve available metagenomes from NCBI Genbank.
#' NCBI Genbank allows users to download entire metagenomes of several
#' metagenome projects. This function downloads available metagenomes that can
#' then be downloaded via \code{\link{getMetaGenomes}}.
#' @param name metagenome name retrieved by \code{\link{listMetaGenomes}}.
#' @param path a character string specifying the location (a folder) in
#' which the corresponding metagenome shall be stored.
#' Default is \code{path} = \code{file.path("_ncbi_downloads","metagenome")}.
#' @param metagenomes.members a tibble of metagenome assemblies,
#' default: dplyr::filter(getMetaGenomeSummary(), organism_name == name)
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # Frist, retrieve a list of available metagenomes
#' listMetaGenomes()
#'
#' # Now, retrieve the 'human gut metagenome'
#' getMetaGenomes(name = "human gut metagenome")
#' }
#' @seealso \code{\link{getMetaGenomeAnnotations}},
#' \code{\link{listMetaGenomes}}
#' @export
getMetaGenomes <-
    function(name,
             path = file.path("_ncbi_downloads", "metagenome"),
             metagenomes.members = dplyr::filter(getMetaGenomeSummary(), organism_name == name)) {
    if (!is.element(name, listMetaGenomes(details = FALSE)))
        stop(
            paste0("Unfortunately, the metagenome '",
            name,
            "' is not available. Please consult the listMetaGenomes() ",
            "function for available metagenomes."), call. = FALSE
        )

    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    if (!is(metagenomes.members, "tbl_df") | nrow(metagenomes.members) == 0) {
      stop("Argument 'metagenomes.members' must be a tibble of nrow > 0")
    }

    file.names <- metagenomes.members$ftp_path
    destfiles <- file.path(path, paste0(
      basename(file.names), "_genomic.fna.gz"))
    download_urls <- paste0(
      file.names,
      "/",
      paste0(
        metagenomes.members$assembly_accession,
        "_",
        metagenomes.members$asm_name,
        "_genomic.fna.gz"
      )
    )
    for (i in seq_len(length(file.names))) {
        custom_download_check_local(download_urls[i], destfiles[i], NULL, db = "genbank")
        docFile_ncbi_metagenome(basename(destfiles[i]), basename(file.names[i]),
                                download_urls[i], path, metagenomes.members[i,])

    }

    message("The metagenome of '", name, "' has been downloaded to '",
            path, "'.")

    file.paths <- file.path(path, list.files(path = path))
    # return only file paths without "*.txt"
    return(file.paths[!unlist(lapply(file.paths, function(x)
        stringr::str_detect(x, "[.]txt")))])
}

