#' @title Helper function for retrieving gff files from ENSEMBL
#' @description This function downloads gff 
#' files of query organisms from ENSEMBL.
#' @param organism scientific name of the organism of interest.
#' @param type specification type.
#' @param id.type id type.
#' @param path location where file shall be stored.
#' @author Hajk-Georg Drost
#' @noRd

getENSEMBL.gtf <-
        function(organism,
                 type = "dna",
                 id.type = "toplevel",
                 path) {
                if (!is.element(type, c("dna", "cds", "pep")))
                        stop("Please a 'type' argument supported by this function: 
                             'dna', 'cds', 'pep'.")
                
            ensembl_summary <-
                suppressMessages(is.genome.available(
                    organism = organism,
                    db = "ensembl",
                    details = TRUE
                ))
            
            if (nrow(ensembl_summary) == 0) {
                message("Unfortunately, organism '",organism,"' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted.")
                return(FALSE)
            }
            
            taxon_id <- assembly <- name <- accession <- NULL
            
            if (nrow(ensembl_summary) > 1) {
                if (is.taxid(organism)) {
                    ensembl_summary <-
                        dplyr::filter(ensembl_summary, taxon_id == as.integer(organism), !is.na(assembly))
                } else {
                    
                    ensembl_summary <-
                        dplyr::filter(
                            ensembl_summary,
                            (name == stringr::str_to_lower(stringr::str_replace_all(organism, " ", "_"))) |
                                (accession == organism),
                            !is.na(assembly)
                        )
                }
            }
            
            new.organism <- ensembl_summary$name[1]
            new.organism <-
                paste0(
                    stringr::str_to_upper(stringr::str_sub(new.organism, 1, 1)),
                    stringr::str_sub(new.organism, 2, nchar(new.organism))
                )
            
            
            rest_url <- paste0(
                "http://rest.ensembl.org/info/assembly/",
                new.organism,
                "?content-type=application/json"
            )
            
            rest_api_status <- test_url_status(url = rest_url, organism = organism)   
            if (is.logical(rest_api_status)) {
                return(FALSE)
            } else {
                
                # construct retrieval query
                ensembl.qry <-
                        paste0(
                                "ftp://ftp.ensembl.org/pub/current_gtf/",
                                stringr::str_to_lower(new.organism),
                                "/",
                                paste0(
                                        stringr::str_to_title(string = new.organism, locale = "en"),
                                        ".",
                                        rest_api_status$default_coord_system_version,
                                        ".",
                                        ensembl_summary$release[1],
                                        ".gtf.gz"
                                )
                        )
                
                if (file.exists(file.path(
                        path,
                        paste0(
                                stringr::str_to_title(string = new.organism, 
                                                      locale = "en"),
                                ".",
                                rest_api_status$default_coord_system_version,
                                ".",
                                ensembl_summary$release[1],
                                "_ensembl",
                                ".gtf.gz"
                        )
                ))) {
                        message(
                                "File ",
                                file.path(
                                        path,
                                        paste0(
                                                stringr::str_to_title(string = new.organism, 
                                                                      locale = "en"),
                                                ".",
                                                rest_api_status$default_coord_system_version,
                                                ".",
                                                ensembl_summary$release[1],
                                                "_ensembl",
                                                ".gtf.gz"
                                        )
                                ),
                                " exists already. Thus, download has been skipped."
                        )
                } else {
                        tryCatch({
                                custom_download(ensembl.qry,
                                                destfile = file.path(
                                                        path,
                                                        paste0(
                                                                stringr::str_to_title(string = new.organism, 
                                                                                      locale = "en"),
                                                                ".",
                                                                rest_api_status$default_coord_system_version,
                                                                ".",
                                                                ensembl_summary$release[1],
                                                                "_ensembl",
                                                                ".gtf.gz"
                                                        )
                                                ),
                                                mode = "wb")
                        }, error = function(e)
                                message(
                                        "Something went wrong while trying to reach the file '",ensembl.qry,"'. This could be due to an instable internet connection or incorrect file path on the ENSEMBL ftp server. Please check if you are able to reach '",ensembl.qry, "' in your web browser.",
                                        " In some cases ENSEMBL released a new database version and path names or the API weren't updated yet. Please give it a few days time or contact the ENSEMBL helpdesk."
                                ))
                }
                
                return(file.path(
                        path,
                        paste0(
                                stringr::str_to_title(string = new.organism, locale = "en"),
                                ".",
                                rest_api_status$default_coord_system_version,
                                ".",
                                ensembl_summary$release[1],
                                "_ensembl",
                                ".gtf.gz"
                        )
                ))
            }
        }
