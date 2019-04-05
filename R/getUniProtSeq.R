#' @title Interface function to UniProt
#' @noRd
getUniProtSeq <-
        function(organism,
                 path = NULL,
                 gunzip = FALSE,
                 update = FALSE) {
                
        organism_new <- stringr::str_replace_all(organism, " ", "%20")
        organism_name_path <- stringr::str_replace_all(organism, " ", "_")
        
        if (file.exists(file.path(tempdir(), "uniprot_info.tsv")) &&
            !update) {
                suppressWarnings(
                        uniprot_species_info <-
                                readr::read_tsv(
                                        file.path(tempdir(), "uniprot_info.tsv"),
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
                        stop(
                                "The API 'https://www.ebi.ac.uk/proteins/api/proteomes'",
                                " does not seem to work properly. Are you connected to the ", " internet? Is the homepage 'https://www.ebi.ac.uk/' currently available?",
                                call. = FALSE
                        ))
                
                name <- upid <- taxonomy <- isReferenceProteome <- isRepresentativeProteome <- NULL
                isRepresentativeProteome <- NULL
                
                uniprot_species_info <- dplyr::filter(uniprot_species_info, stringr::str_detect(name, organism))
                
                if (nrow(uniprot_species_info) > 1) {
                        # message("There are more than one entry for '",organism,"'.",
                        #         " Please select the one below that you prefer and re-run this function using the full name you chose.")
                        # message("\n")
                        # message("Options are: ", paste0("organism = '",uniprot_species_info$name,"', "),".")
                        
                        uniprot_species_info <- uniprot_species_info[1, ]
                }
                
                uniprot_species_info <-
                        dplyr::select(
                                uniprot_species_info,
                                name,
                                upid,
                                taxonomy,
                                isReferenceProteome,
                                isRepresentativeProteome
                        )
                
                readr::write_tsv(uniprot_species_info,
                                 file.path(tempdir(), "uniprot_info.tsv"))
                
        }
        
        
        if (nrow(uniprot_species_info) > 0) {
                
                #organism_new <- stringr::str_replace_all(uniprot_species_info$name, " ", "%20")
                
                query <-
                        paste0(
                                "http://www.uniprot.org/uniprot/?query=organism:%22",
                                organism_new,
                                "%22&sort=score&columns=id,protein%20names,genes,sequence&format=fasta"
                        )
                
                tryCatch({organism_fasta_file <-
                        Biostrings::readBStringSet(query)}, error = function(e)
                                stop(
                                        "Something went wrong ... The API ", query, " could not be used to retrieve a fasta file for '", organism,"'. Could it be that your internet connection was interrupted?",
                                        call. = FALSE
                                ))
                
                Biostrings::writeXStringSet(organism_fasta_file,
                                            filepath = file.path(
                                                    ifelse(is.null(path), getwd(), path),
                                                    paste0(
                                                            organism_name_path,
                                                            "_protein_uniprot.faa.gz"
                                                    )
                                            ), compress = TRUE)
                

                
                docFile(
                        file.name = paste0(organism_name_path, "_protein_uniprot.faa.gz"),
                        organism  = uniprot_species_info$name,
                        url       = query,
                        database  = "uniprot",
                        path      = ifelse(is.null(path), getwd(), path),
                        refseq_category = "",
                        assembly_accession = "",
                        bioproject = "",
                        biosample = "",
                        taxid = uniprot_species_info$taxonomy,
                        infraspecific_name = "",
                        version_status = "",
                        release_type = "",
                        genome_rep = "",
                        seq_rel_date = "",
                        submitter = ""
                )
                
                if (!gunzip) {
                        message(
                                paste0(
                                        "The proteome of '",
                                        organism,
                                        "' has been downloaded to '",
                                        ifelse(is.null(path), getwd(), path),
                                        "' and has been named '",
                                        paste0(organism_name_path, "_protein_uniprot.faa.gz"),
                                        "' ."
                                )
                        )
                }
                
                if (gunzip) {
                        message(
                                paste0(
                                        "The proteome of '",
                                        organism,
                                        "' has been downloaded to '",
                                        ifelse(is.null(path), getwd(), path),
                                        "' and has been named '",
                                        paste0(organism_name_path, "_protein_uniprot.faa"),
                                        "' ."
                                )
                        )
                }
                
                if (gunzip) {
                        message("Unzipping downloaded file ...")
                        R.utils::gunzip(file.path(
                                ifelse(is.null(path), getwd(), path),
                                paste0(
                                        organism_name_path,
                                        "_protein_uniprot.faa.gz"
                                )
                        ), destname = file.path(
                                ifelse(is.null(path), getwd(), path),
                                paste0(
                                        organism_name_path,
                                        "_protein_uniprot.faa"
                                )
                        ))
                        return(file.path(
                                ifelse(is.null(path), getwd(), path),
                                paste0(
                                        organism_name_path,
                                        "_protein_uniprot.faa"
                                )
                        ))
                } else {
                        return(file.path(
                                ifelse(is.null(path), getwd(), path),
                                paste0(
                                        organism_name_path,
                                        "_protein_uniprot.faa.gz"
                                )
                        ))
                }
                
        } else {
                warning("Unfortunately, no entry for '",organism,"' has been found. Are you certain that you typed the scientific name correctly, e.g. Homo sapiens (capital letter in the first name)?", call. = FALSE)
                return(NA)
        }
}




