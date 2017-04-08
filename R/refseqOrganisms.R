#' @title Retrieve All Organism Names Stored on refseq
#' @description This function extracts all organism names (scientific names)
#' for which genomes, proteomes, and CDS files are stored on 
#' the NCBI refseq server.
#' @author Hajk-Georg Drost
#' @export

refseqOrganisms <- function() {
    refseq_kingdoms <-   c(
        "archaea",
        "bacteria",
        "fungi",
        "invertebrate",
        "plant",
        "protozoa",
        "vertebrate_mammalian",
        "vertebrate_other"
    )
    
    
    all_refseqOrgs <-
        as.vector(unlist(lapply(refseq_kingdoms, function(kingdom)
            strsplit(
                RCurl::getURL(
                    paste0(
                        "ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",
                        kingdom,
                        "/"
                    ),
                    ftp.use.epsv = FALSE,
                    dirlistonly = TRUE
                ),
                "\n"
            ))))
    
    
    all_refseqOrgs <-
        stringr::str_replace(all_refseqOrgs, "_", " ")
    
    return(all_refseqOrgs)
}
