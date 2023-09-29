#' @title Retrieve and summarise the assembly_summary.txt files from
#' NCBI for all kingdoms
#' @description Retrieval function of the assembly_summary.txt file
#' from NCBI for all kingdoms.
#' The assembly_summary.txt files store available species on NCBI.
#' @param db database name. E.g. \code{refseq} or \code{genbank}.
#' @param skip_bacteria Due to its enormous dataset size (> 700MB as of July 2023),
#' the bacterial summary file will not be loaded by default anymore. If users
#' wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
#' bacterial summary file will be downloaded.
#' @param file path, local path to total summary file, default is in tmp folder.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # This example will run the default version of this function
#' # whereby information for Bacteria are not downloaded
#' test <- getKingdomAssemblySummary(db = "genbank", skip_bacteria = TRUE)
#' test
#' # Users can then retrieve information for Bacteria by skip_bacteria = FALSE
#' test2 <- getKingdomAssemblySummary(db = "genbank", skip_bacteria = FALSE)
#' test2
#' }
#' @seealso \code{\link{getSummaryFile}}, \code{\link{getMetaGenomeSummary}}, \code{\link{get.ensembl.info}}
#' @export

getKingdomAssemblySummary <- function(db, skip_bacteria = TRUE,
                                      file = assemblies_info_path(db)
                                      ) {
    if (!is.element(db, c("refseq", "genbank")))
        stop("Please select one of the available data bases:
             'refseq' or 'genbank'")

    if (is.element(db, c("refseq", "genbank"))) {
        # if AssemblyFilesAllKingdoms.txt file was already generated/downloaded
        # then use the local version stored in temp()
        if (file.exists(file) & (skip_bacteria | file.exists(assemblies_info_path(db, "bacteria")))) {
          AssemblyFilesAllKingdoms <- suppressWarnings(read_all_kingdoms_assemblies_info(file))
        } else {
            # otherwise download all assembly_summary.txt files for all kingdoms
            # and store the AssemblyFilesAllKingdoms.txt file locally
            # retrieve the assembly_summary.txt files for all kingdoms
            cat("It seems that this is the first time you run this command for",db,".")
            cat("\n")
            message("Thus, 'assembly_summary.txt' files for all kingdoms will be retrieved from ",db,". ")
            message("Don't worry this has to be done only once if you don't restart your R session.")
            message("\n")
            kgdoms <- getKingdoms(db = db)
            storeAssemblyFiles <- vector("list", length(kgdoms))

            if (skip_bacteria){
                cat( "Due to its extended dataset size (GenBank: >700 MB, RefSeq: >150 MB) Kingdom 'bacteria' will not be downloaded by default anymore. To also include 'bacteria' please specify the argument 'skip_bacteria = FALSE'" )
                cat("\n")
                cat("\n")
            }

            for (i in seq_along(kgdoms)) {
                if (kgdoms[i] == "bacteria" && (db == "genbank" || db == "refseq") && skip_bacteria){
                    cat("--------> Skipping bacteria download .....")
                    cat("\n")
                    cat("\n")
                } else {
                    cat("-> Starting download for:", kgdoms[i])
                    cat("\n")
                    storeAssemblyFiles[i] <-
                        list(getSummaryFile(db = db, kingdom = kgdoms[i]))
                }
            }

            AssemblyFilesAllKingdoms <-
                dplyr::bind_rows(storeAssemblyFiles)

            readr::write_tsv(AssemblyFilesAllKingdoms, file)

            message("\n")
            message("Completed!")
            message("Now continue with species download ...")
        }
    }

    orgs <-
        stringr::str_replace_all(AssemblyFilesAllKingdoms$organism_name,
                                 "\\(", "")
    orgs <- stringr::str_replace_all(orgs, "\\)", "")

    AssemblyFilesAllKingdoms <-
        dplyr::mutate(AssemblyFilesAllKingdoms, organism_name = orgs)

    return(AssemblyFilesAllKingdoms)
}
