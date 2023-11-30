#' @title List All Available Genomes either by kingdom, group, or subgroup
#' @description This function retrieves the names of all genomes available on
#' the NCBI ftp:// server and stores the results in a file named 'overview.txt'
#' inside the directory _ncbi_downloads' that is built inside the workspace.
#' @param db a character string specifying the database for which genome
#' availability shall be checked. Available options are:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' }
#' @param type a character string specifying a potential filter of available
#' genomes. Available options are:
#' \itemize{
#' \item \code{type = "all", no subset}
#' \item \code{type = "kingdom", subset on kingdom}
#' \item \code{type = "group", subset on group}
#' \item \code{type = "subgroup", subset on subgroup}
#' }
#' @param subset a character string or character vector specifying a subset of
#' \code{type}. E.g. if users are interested in retrieving all
#' \code{Eukaryota} species, they can specify: \code{type = "kingdom"} and
#' \code{subset = "Eukaryota"}.
#' @param details a boolean value specifying whether only the scientific names
#' of stored genomes shall be returned (details = FALSE) or all information such
#' as
#' \itemize{
#' \item \code{organism_name}
#' \item \code{kingdoms}
#' \item \code{group}
#' \item \code{subgroup}
#' \item \code{file_size_MB}, etc.
#' }
#' @inheritParams getENSEMBLInfo
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI
#' and creates a directory '_ncbi_downloads' in the \code{temdir()}
#' folder to store the overview.txt file for future processing. In case the
#' overview.txt file already exists within the '_ncbi_downloads' folder and is
#' accessible within the workspace, no download process will be performed again.
#' @note
#' Please note that the ftp:// connection relies on the NCBI or ENSEMBL server
#' and cannot be accurately accessed via a proxy.
#' @examples
#' \dontrun{
#' # print details for refseq
#' listGenomes(db = "refseq")
#' # print details for all plants in refseq
#' listGenomes(db = "refseq", type = "kingdom")
#' # print details for all plant groups in refseq
#' listGenomes(db = "refseq", type = "group")
#' # print details for all plant subgroups in refseq
#' listGenomes(db = "refseq", type = "subgroup")
#' # Ensembl
#' listGenomes(db = "ensembl", type = "kingdom", subset = "EnsemblVertebrates")
#' }
#' @export
listGenomes <- function(db = "refseq", type = "all", subset = NULL,
                        details = FALSE, update = FALSE, skip_bacteria = FALSE) {

  if (!is.element(db, c("refseq", "genbank", "ensembl")))
    stop(
      "Please specify a database that is supported by this function.
              E.g. 'refseq', 'genbank', or 'ensembl'.",
      call. = FALSE
    )

  if (!is.element(type, c("all", "kingdom", "group", "subgroup")))
    stop(
      "Please specify a type that is supported by this function.
              E.g. 'all', kingdom', 'group', or 'subgroup'.",
      call. = FALSE
    )
  stopifnot(is.logical(details))

  if (is.element(db, c("refseq", "genbank"))) {
    # retrieve genome report overview file
    ncbi_overview <- getGENOMEREPORT()

    # get Kingdom Assembly Summary file
    AssemblyFilesAllKingdoms <-
      getKingdomAssemblySummary(db = db, skip_bacteria = skip_bacteria)

    # join tables to retrieve kingdom, group, subgroup information for
    # refseq/genbank organisms
    genomes_info <-
      dplyr::inner_join(AssemblyFilesAllKingdoms, ncbi_overview,
                        by = "organism_name")

    if (type == "all") {
      if (!is.null(subset)) {
        warning(
          "For option type = 'all' no subset can be specified.",
          " Please select another type and then specify subset = '",
          subset,
          "'.",
          call. = FALSE
        )
      }

      if (details) {
        return(tibble::as_tibble(genomes_info))
      }
      if (!details) {
        return(unique(genomes_info$organism_name))
      }
    }

    set <-
      if (type == "kingdom") {
        genomes_info$kingdoms
      } else if (type == "group") {
        genomes_info$group
      } else if (type == "subgroup") {
        genomes_info$subgroup
      }
    names <- genomes_info$organism_name
  }

  if (db %in% c("ensembl", "ensemblgenomes")) {
    if (!is.element(type, c("all", "kingdom")))
      stop(
        "Unfortunately, ENSEMBL only provides kingdom information and no group or subgroup information.",
        call. = FALSE
      )

    genomes_info <- getENSEMBLInfo(update, subset)
    set <- genomes_info$division
    names <- genomes_info$name
  }

  if (!is.null(subset) && !all(subset %in% set))
    stop(genomes_wrong_subset_message(subset, set),
         call. = FALSE)

  if (details) {
    if (!is.null(subset)) {
      return(genomes_info[set %in% subset,])
    } else {
      return(tibble::as_tibble(genomes_info))
    }
  }

  if (!details) {
    if (!is.null(subset)) {
      return(unique(names[set %in% subset]))
    } else {
      return(unique(names))
    }
  }
}
