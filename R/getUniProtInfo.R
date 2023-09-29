#' @export
getUniProtInfo <- function(organism) {
  organism_new <- stringr::str_replace_all(organism, " ", "%20")
  organism_name_path <- stringr::str_replace_all(organism, " ", "_")
  
  if (file.exists(file.path(tempdir(), paste0(organism_name_path, "_uniprot_info.tsv"))) &&
      !update) {
    suppressWarnings(
      uniprot_species_info <-
        readr::read_tsv(
          file.path(tempdir(), paste0(organism_name_path, "_uniprot_info.tsv")),
          col_names = TRUE,
          col_types = readr::cols(
            name = readr::col_character(),
            upid = readr::col_integer(),
            taxonomy = readr::col_integer(),
            isReferenceProteome = readr::col_logical(),
            isRepresentativeProteome = readr::col_logical()
          )
        )
    )
    
  } else {
    tryCatch({
      uniprot_species_info <-
        tibble::as_tibble(jsonlite::fromJSON(
          paste0(
            "https://www.ebi.ac.uk/proteins/api/proteomes?offset=0&size=-1&name=",
            organism_new
          )
        ))
    }, error = function(e)
      message(
        "Something went wrong when trying to access the API 'https://www.ebi.ac.uk/proteins/api/proteomes'",
        " Sometimes the internet connection isn't stable and re-running the function might help. Is it possible to access the homepage 'https://www.ebi.ac.uk/' through your browser?"
      ))}
    
    return(uniprot_species_info)
  }
  