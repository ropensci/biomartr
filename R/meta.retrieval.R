#' @title Perform Meta-Genome Retieval
#' @description Download genomes, proteomes, or CDS of all species within a kingdom of life.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other".
#' @param type type of sequences that shall be retrieved. Either \code{genome}, \code{proteome}, or \code{CDS}.
#' @param out.folder path to the folder in which downloaded genomes shall be stored. By default the
#' kingdom name is used to name the output folder.
#' @author Hajk-Georg Drost
#' @details This function aims to perform bulk retrieval of the genomes of species
#' that belong to the same kingdom of life.
#' @examples 
#' \dontrun{
#' # download all vertebrate genomes
#' meta.retrieval(kingdom = "vertebrate_mammalian", type = "genome")
#' }
#' @export

meta.retrieval <- function(kingdom, type = "genome", out.folder = NULL){
        
        subfolders <- getKingdoms()
        
        if(!is.element(kingdom,subfolders))
                stop (paste0("Please select a valid kingdom: ",paste0(subfolders,collapse = ", ")))
        
        if (!is.element(type, c("genome","proteome","CDS")))
                stop ("Please choose either type: 'genome', 'proteome', or 'CDS'")
        
        getOrganisms <- try (RCurl::getURL(paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/",kingdom,"/"), 
                                          ftp.use.epsv = FALSE, dirlistonly = TRUE))
        FilterOrganisms <- strsplit(getOrganisms,"\n")
        FinalOrganisms <- stringr::str_replace(unlist(FilterOrganisms),"_"," ")
        FinalOrganisms <- FinalOrganisms[-which(is.element(FinalOrganisms, c("assembly summary_historical.txt","assembly summary.txt")))]
        
        cat("\n")
        cat("Starting meta retrieval of all ",type,"s for ",kingdom,".")
        cat("\n")
        
        if (type == "genome"){
                if (is.null(out.folder)){
                        for (i in seq_len(length(FinalOrganisms))){
                                getGenome( db       = "refseq",
                                           kingdom  = kingdom,
                                           organism = FinalOrganisms[i],
                                           path     = kingdom)
                        }
                }
                
                if (!is.null(out.folder)){
                        for (i in seq_len(length(FinalOrganisms))){
                                getGenome( db       = "refseq",
                                           kingdom  = kingdom,
                                           organism = FinalOrganisms[i],
                                           path     = out.folder)
                        }
                }
        }
        
        if (type == "proteome"){
                if (is.null(out.folder)){
                        for (i in seq_len(length(FinalOrganisms))){
                                getProteome( db       = "refseq",
                                           kingdom  = kingdom,
                                           organism = FinalOrganisms[i],
                                           path     = kingdom)
                        }
                }
                
                if (!is.null(out.folder)){
                        for (i in seq_len(length(FinalOrganisms))){
                                getProteome( db       = "refseq",
                                           kingdom  = kingdom,
                                           organism = FinalOrganisms[i],
                                           path     = out.folder)
                        }
                }
        }
        
        if (type == "CDS"){
                if (is.null(out.folder)){
                        for (i in seq_len(length(FinalOrganisms))){
                                getCDS( db       = "refseq",
                                             kingdom  = kingdom,
                                             organism = FinalOrganisms[i],
                                             path     = kingdom)
                        }
                }
                
                if (!is.null(out.folder)){
                        for (i in seq_len(length(FinalOrganisms))){
                                getCDS( db       = "refseq",
                                             kingdom  = kingdom,
                                             organism = FinalOrganisms[i],
                                             path     = out.folder)
                        }
                }
        }
        cat("\n")
        cat("Finished meta retieval process.")
}




