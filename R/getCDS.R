#' @title Coding Sequence Retrieval
#' @description This function retrieves a fasta-file storing the CDS files of the genome of an organism of interest and stores
#' this file in the folder '_ncbi_downloads/CDS'.
#' @param db a character string specifying the database from which the CDS file shall be retrieved: \code{refseq}.
#' Right now only the ref seq database is included. Later version of \pkg{biomartr} will also allow
#' sequence retrieval from additional databases.
#' @param organism a character string specifying the scientific name of the organism of interest, e.g. 'Arabidopsis thaliana'.
#' @param path a character string specifying the location (a folder) in which the corresponding
#' CDS file shall be stored. Default is \code{path} = \code{file.path("_ncbi_downloads","CDS")}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' 
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#' 
#' and creates a directory '_ncbi_downloads/CDS' to store
#' the genome of interest as CDS fasta file for future processing.
#' In case the corresponding fasta file already exists within the
#' '_ncbi_downloads/CDS' folder and is accessible within the workspace,
#' no download process will be performed. So the folder can delete when the corresponding
#' CDS file shall be downloaded again. 
#' 
#' @return A data.table storing the geneids in the first column and the DNA dequence in the second column.
#' @examples \dontrun{
#' 
#' # download the genome of Arabidopsis thaliana from refseq
#' # and store the corresponding genome CDS file in '_ncbi_downloads/CDS'
#' file_path <- getCDS( db       = "refseq", 
#'              organism = "Arabidopsis thaliana", 
#'              path     = file.path("_ncbi_downloads","CDS"))
#' 
#' Ath_CDS <- read_cds(file_path, format = "fasta")
#' 
#' }
#' @references 
#' 
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq}
#'  
#' \url{http://www.ncbi.nlm.nih.gov/refseq/about/}
#' 
#' @seealso \code{\link{getGenome}}, \code{\link{getProteome}}, \code{\link{meta.retrieval}}, \code{\link{read_cds}}
#' @export

getCDS <- function(db = "refseq", organism, path = file.path("_ncbi_downloads","CDS")){
    
    if (!is.element(db, c("refseq", "genbank", 'ensembl')))
        stop ("Please select one of the available data bases: 'refseq', 'genbank', or 'ensembl'.")
    
    # get Kingdom Assembly Summary file
    AssemblyFilesAllKingdoms <- getKingdomAssemblySummary(db = db)
    
    # test wheter or not genome is available
    is.genome.available(organism = organism, db = db)
    
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    
    organism_name <- refseq_category <- version_status <- NULL
    
    FoundOrganism <- dplyr::filter(
        AssemblyFilesAllKingdoms,
        stringr::str_detect(organism_name, organism),
        ((refseq_category == "representative genome") ||
             (refseq_category == "reference genome")
        ),
        (version_status == "latest")
    )
    
    if (nrow(FoundOrganism) > 1) {
        warnings(
            "More than one entry has been found for '",
            organism,
            "'. Only the first entry '",
            FoundOrganism[1, 1],
            "' has been used for subsequent genomic CDS retrieval."
        )
        FoundOrganism <- FoundOrganism[1,]
    }
    
    organism <- stringr::str_replace(organism, " ", "_")
    
    download_url <-
        paste0(
            FoundOrganism$ftp_path,
            "/",
            paste0(
                FoundOrganism$assembly_accession,
                "_",
                FoundOrganism$asm_name,
                "_cds_from_genomic.fna.gz"
            )
        )
    
    if (nrow(FoundOrganism) == 1) {
        tryCatch({utils::capture.output(downloader::download(
            download_url,
            destfile = file.path(path, paste0(
                organism, "_cds_from_genomic.fna.gz"
            )),
            mode = "wb"
        ))}, error = function(e)
            stop(
                "The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/' cannot be reached. Are you connected to the internet? Is the the FTP site '",download_url,"' currently available?", call. = FALSE
            ))
        
        docFile(
            file.name = paste0(organism, "_cds_from_genomic.fna.gz"),
            organism  = organism,
            url       = download_url,
            database  = db,
            path      = path,
            refseq_category = FoundOrganism$refseq_category,
            assembly_accession = FoundOrganism$assembly_accession,
            bioproject = FoundOrganism$bioproject,
            biosample = FoundOrganism$biosample,
            taxid = FoundOrganism$taxid,
            infraspecific_name = FoundOrganism$infraspecific_name,
            version_status = FoundOrganism$version_status,
            release_type = FoundOrganism$release_type,
            genome_rep = FoundOrganism$genome_rep,
            seq_rel_date = FoundOrganism$seq_rel_date,
            submitter = FoundOrganism$submitter
        )
        
        # NCBI limits requests to three per second
        Sys.sleep(0.33)
        
        print(
            paste0(
                "The genomic CDS of '",
                organism,
                "' has been downloaded to '",
                path,
                "' and has been named '",
                paste0(organism, "_cds_from_genomic.fna.gz"),
                "' ."
            )
        )
        
        return(file.path(path, paste0(
            organism, "_cds_from_genomic.fna.gz"
        )))
    } else {
        warning (
            "File: ",
            download_url,
            " could not be loaded properly... Are you connected to the internet?"
        )
    }
}








