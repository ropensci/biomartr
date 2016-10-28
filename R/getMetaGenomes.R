#' @title Retrieve metagenomes from NCBI Genbank
#' @description Retrieve available metagenomes from NCBI Genbank. NCBI Genbank allows users
#' to download entire metagenomes of several metagenome projects. This function downloads
#'  available metagenomes that can then be downloaded via \code{\link{getMetaGenomes}}.
#' @param name metagenome name retrieved by \code{\link{listMetaGenomes}}.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' metagenome shall be stored. Default is \code{path} = \code{file.path("_ncbi_downloads","metagenome")}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # Frist, retrieve a list of available metagenomes
#' listMetaGenomes()
#' 
#' # Now, retrieve the 'human gut metagenome'
#' getMetaGenomes(name = "human gut metagenome")
#' }
#' @seealso \code{\link{getMetaGenomeAnnotations}}, \code{\link{listMetaGenomes}}  
#' @export

getMetaGenomes <- function(name, path = file.path("_ncbi_downloads","metagenome")) {
    
    if (!is.element(name, listMetaGenomes(details = FALSE)))
        stop("Unfortunately, the metagenome '",name,"' is not available. Please consult the listMetaGenomes() function for available metagenomes.")
    
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    
    organism_name <- NULL
    
    # retrieve metagenomes assembly_summary.txt file
    mgs <- getMetaGenomeSummary()
    
    metagenomes.members <- dplyr::filter(mgs, organism_name == name)
    file.names <- metagenomes.members$ftp_path
    
    for (i in seq_len(length(file.names))) {
        
        download_url <-
            paste0(
                file.names[i],
                "/",
                paste0(
                    metagenomes.members$assembly_accession[i],
                    "_",
                    metagenomes.members$asm_name[i],
                    "_genomic.fna.gz"
                )
            )
        
            tryCatch({utils::capture.output(downloader::download(
                download_url,
                destfile = file.path(path, paste0(basename(file.names[i]), "_genomic.fna.gz")),
                mode = "wb"
            ))}, error = function(e)
                stop(
                    "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' cannot be reached. Are you connected to the internet? Is the the FTP site '",download_url,"' currently available?"
                ))
            
            docFile(
                file.name = paste0(basename(file.names[i]), "_genomic.fna.gz"),
                organism  = basename(file.names[i]),
                url       = download_url,
                database  = "Genbank metagenomes",
                path      = path,
                refseq_category = metagenomes.members$refseq_category[i],
                assembly_accession = metagenomes.members$assembly_accession[i],
                bioproject = metagenomes.members$bioproject[i],
                biosample = metagenomes.members$biosample[i],
                taxid = metagenomes.members$taxid[i],
                infraspecific_name = metagenomes.members$infraspecific_name[i],
                version_status = metagenomes.members$version_status[i],
                release_type = metagenomes.members$release_type[i],
                genome_rep = metagenomes.members$genome_rep[i],
                seq_rel_date = metagenomes.members$seq_rel_date[i],
                submitter = metagenomes.members$submitter[i]
            )
            
            # NCBI limits requests to three per second
            Sys.sleep(0.33)
            
}
    
    print(
        paste0(
            "The metagenome of '",
            name,
            "' has been downloaded to '",
            path,
            "'."
        )
    )
    
    file.paths <- file.path(path,list.files(path = path))
    # return only file paths without "*.txt"
    return(file.paths[!unlist(sapply(file.paths, function(x) stringr::str_detect(x,"[.]txt")))])
}
