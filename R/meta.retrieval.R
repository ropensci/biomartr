#' @title Perform Meta-Genome Retieval
#' @description Download genomes, proteomes, or CDS of all species within a kingdom of life.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other".
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{refseq} or \code{genbank}.
#' @param type type of sequences that shall be retrieved. Either \code{genome}, \code{proteome}, or \code{CDS}.
#' @param out.folder path to the folder in which downloaded genomes shall be stored. By default the
#' kingdom name is used to name the output folder.
#' @author Hajk-Georg Drost
#' @details This function aims to perform bulk retrieval of the genomes of species
#' that belong to the same kingdom of life.
#' @examples 
#' \dontrun{
#' # get all available kingdoms
#' getKingdoms()
#' 
#' # download all vertebrate genomes from refseq
#' meta.retrieval(kingdom = "vertebrate_mammalian", db = "refseq", type = "genome")
#' 
#' # download all vertebrate genomes from genbank
#' meta.retrieval(kingdom = "vertebrate_mammalian", db = "genbank", type = "genome")
#' }
#' @export

meta.retrieval <- function(kingdom, 
                           db         = "refseq", 
                           type       = "genome", 
                           out.folder = NULL){
        
    subfolders <- getKingdoms()
    
    if (!is.element(kingdom, subfolders))
        stop (paste0(
            "Please select a valid kingdom: ",
            paste0(subfolders, collapse = ", ")
        ))
    
    if (!is.element(type, c("genome", "proteome", "CDS")))
        stop ("Please choose either type: 'genome', 'proteome', or 'CDS'")
    
    if (!is.element(db, c("refseq", "genbank")))
        stop ("Please select einter 'db = 'refseq'' or 'db = 'genbank''")
    
    if ((type == "CDS") && (db == "genbank"))
        stop ("Genbank does not store CDS data. Please choose 'db = 'refseq''.")
    
    
    getOrganisms <-
        try (RCurl::getURL(
            paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/", db, "/", kingdom, "/"),
            ftp.use.epsv = FALSE,
            dirlistonly = TRUE
        ))
    FilterOrganisms <- strsplit(getOrganisms, "\n")
    FinalOrganisms <-
        stringr::str_replace(unlist(FilterOrganisms), "_", " ")
    FinalOrganisms <-
        FinalOrganisms[-which(is.element(
            FinalOrganisms,
            c("assembly summary_historical.txt", "assembly summary.txt")
        ))]
    
    cat("\n")
    cat(paste0("Starting meta retrieval of all ", type, "s for ", kingdom, "."))
    cat("\n")
    
    if (type == "genome") {
        if (is.null(out.folder)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getGenome(db       = db,
                          organism = FinalOrganisms[i],
                          path     = kingdom)
            }
        }
        
        if (!is.null(out.folder)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getGenome(db       = db,
                          organism = FinalOrganisms[i],
                          path     = out.folder)
            }
        }
    }
    
    if (type == "proteome") {
        if (is.null(out.folder)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getProteome(db       = db,
                            organism = FinalOrganisms[i],
                            path     = kingdom)
            }
        }
        
        if (!is.null(out.folder)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getProteome(db       = db,
                            organism = FinalOrganisms[i],
                            path     = out.folder)
            }
        }
    }
    
    if (type == "CDS") {
        if (is.null(out.folder)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getCDS(db       = db,
                       organism = FinalOrganisms[i],
                       path     = kingdom)
            }
        }
        
        if (!is.null(out.folder)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getCDS(db       = db,
                       organism = FinalOrganisms[i],
                       path     = out.folder)
            }
        }
    }
    cat("\n")
    cat("Finished meta retieval process.")
}




