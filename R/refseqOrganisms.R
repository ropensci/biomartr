#' @title Retrieve All Organism Names Stored on refseq
#' @description This function extracts all organism names (scientific names)
#' for which genomes, proteomes, and CDS files are stored on
#' the NCBI refseq server.
#' @return a character vector of html from kingdom URLs
#' @author Hajk-Georg Drost
#' @export
refseqOrganisms <- function() {


    all_refseqOrgs <-
        as.vector(unlist(lapply(refseq_kingdoms_url(), function(kingdom)
            strsplit(
                RCurl::getURL(
                    kingdom,
                    ftp.use.epsv = FALSE,
                    dirlistonly = TRUE
                ),
                "\n"
            ))))


    all_refseqOrgs <-
        stringr::str_replace(all_refseqOrgs, "_", " ")

    return(all_refseqOrgs)
}
