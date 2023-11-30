please_cite_biomartr <- function(mute_citation = FALSE) {
  if (!mute_citation) {
    cat("\n")
    cat("\n")
    cat("Please cite: Drost HG, Paszkowski J. Biomartr: genomic data retrieval with R. Bioinformatics (2017) 33(8): 1216-1217. doi:10.1093/bioinformatics/btw821.")
    cat("\n")
  }
}

organism_no_hit_message_zero <- function(organism, db) {
  message(
    "Unfortunatey, no entry for '",
    organism,
    "' was found in the '",
    db,
    "' database. ",
    "Please consider specifying ",
    paste0("'db = ", dplyr::setdiff(
      db_hosts(), db
    ), collapse = "' or "),
    "' to check whether '",organism,"' is available in these databases."
  )
}

organism_no_hit_message_more_than_one <- function(organism, db) {
  message("More than one entry was found for '", organism, "'.",
          " Please consider to run the function 'is.genome.available()' and specify 'is.genome.available(organism = ",
          organism, ", db = ",db, ", details = TRUE)'.",
          " This will allow you to select the 'assembly_accession' identifier that can then be ",
          "specified in all get*() functions.")
}

genomes_wrong_subset_message <- function(subset, set) {
  paste0("Unfortunately, not all members of your specified subset '",
         paste0(subset, collapse = ", '"),
         "' could be found. Must be subset of:\n",
        paste0(unique(set), collapse = ", "))
}

wrong_mart_message <- function(mart, dataset) {
  dataset <- ifelse(!is.null(dataset), paste0("or dataset '", dataset,"'"), "")
  paste(
    "Your input mart '", mart, "'", dataset,
    "could not be found. Please use getMarts() to choose from available marts."
  )
}
