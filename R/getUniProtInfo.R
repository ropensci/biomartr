#' Get uniprot info from organism
#'
#' @param organism character, name of organism
#' @export
getUniProtInfo <- function(organism, path = cachedir(), update = TRUE) {
  organism_new <- stringr::str_replace_all(organism, " ", "%20")
  organism_name_path <- stringr::str_replace_all(organism, " ", "_")

  file <- file.path(cachedir(), paste0(organism_name_path, "_uniprot_info.tsv"))
  ebi_url <- "https://www.ebi.ac.uk/"
  ebi_url_proteome <- paste0(ebi_url, "proteins/api/proteomes")
  if (file.exists(file) && !update) {
    suppressWarnings(uniprot_species_info <- read_uniprot_info(file))

  } else {
    tryCatch({
      uniprot_species_info <-
        tibble::as_tibble(jsonlite::fromJSON(
          paste0(ebi_url_proteome, "?offset=0&size=-1&name=", organism_new)
        ))
      uniprot_species_info <- write_uniprot_info(file, uniprot_species_info,
                                                 organism)
    }, error = function(e)
      message(
        "Something went wrong when trying to access the API '", ebi_url_proteome, "'",
        " Sometimes the internet connection isn't stable and re-running the function might help. Is it possible to access the homepage '",
        ebi_url, "' through your browser?"
      ))}

    return(uniprot_species_info)
}

read_uniprot_info <- function(file) {
  readr::read_tsv(
    file,
    col_names = TRUE,
    col_types = readr::cols(
      name = readr::col_character(),
      upid = readr::col_integer(),
      taxonomy = readr::col_integer(),
      isReferenceProteome = readr::col_logical(),
      isRepresentativeProteome = readr::col_logical()
    )
  )
}

write_uniprot_info <- function(file, uniprot_species_info, organism) {
  name <- upid <- taxonomy <- isReferenceProteome <-
    isRepresentativeProteome <- NULL

  uniprot_species_info <-
    dplyr::filter(uniprot_species_info, stringr::str_detect(name, organism))

  if (nrow(uniprot_species_info) > 1) {
    # message("There are more than one entry for '",organism,"'.",
    #         " Please select the one below that you prefer and re-run this function using the full name you chose.")
    # message("\n")
    # message("Options are: ", paste0("organism = '",uniprot_species_info$name,"', "),".")
    # Subset to valid reference
    is_reference <- uniprot_species_info$isReferenceProteome
    if (any(is_reference)) {
      uniprot_species_info <- uniprot_species_info[is_reference, ]
    }

    most_expansive_assembly <- which.max(sapply(uniprot_species_info$dbReference, nrow))
    uniprot_species_info <- uniprot_species_info[most_expansive_assembly,]
  }

  if (nrow(uniprot_species_info) == 0) {
    warning("Could not find a valid reference assembly id for uniprot for this species!")
    return(uniprot_species_info)
  }

  uniprot_species_info <-
    dplyr::select(
      uniprot_species_info,
      name,
      upid,
      taxonomy,
      isReferenceProteome,
      isRepresentativeProteome,
      superregnum
    )

  readr::write_tsv(uniprot_species_info, file)
  return(uniprot_species_info)
}
