#' @title Retrieve ENSEMBL info file
#' @description Retrieve species and genome information from
#' http://rest.ensembl.org/info/species?content-type=application/json/.
#' @author Hajk-Georg Drost
#' @export
getENSEMBLInfo <- function() {
    ENSEMBLInfoTable <- get.ensembl.info(update = TRUE)
    return(ENSEMBLInfoTable)
}

ensembl_assembly_hits <- function(organism) {
  ensembl_summary <-
    suppressMessages(is.genome.available(
      organism = organism,
      db = "ensembl",
      details = TRUE
    ))

  if (nrow(ensembl_summary) == 0) {
    message("Unfortunately, organism '",organism,"' does not exist in this database. ",
            "Could it be that the organism name is misspelled? Thus, download has been omitted.")
    return(FALSE)
  }
  ensembl_summary <- ensembl_summaries_filter(ensembl_summary, organism)
  return(ensembl_summary)
}

ensembl_summaries_filter <- function(ensembl_summary, organism) {
  if (nrow(ensembl_summary) > 1) {
    taxon_id <- assembly <- name <- accession <- NULL
    ensembl_summary_copy <- ensembl_summary
    if (is.taxid(organism)) {
      ensembl_summary <-
        dplyr::filter(ensembl_summary, taxon_id == as.integer(organism),
                      !is.na(assembly))
    } else {
      ensembl_summary <-
        dplyr::filter(
          ensembl_summary,
          (name == lower_cap_underscore_organism_name(organism)) |
            (accession == organism),
          !is.na(assembly)
        )
      if (nrow(ensembl_summary) == 0) {
        ensembl_summary <- dplyr::filter(ensembl_summary_copy,
                                         !is.na(assembly))
      }
    }
    if (nrow(ensembl_summary) == 0) {
      print(ensembl_summary_copy)
      stop("All assemblies removed by filter (more info in lines above)")
    }
  }
  return(ensembl_summary)
}
