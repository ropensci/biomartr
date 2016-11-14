#' @title Perform Meta-Genome Retieval
#' @description Download genomes, proteomes, or CDS of all species within a kingdom of life.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other".
#' Available kingdoms can be retrieved with \code{\link{getKingdoms}}.
#' @param db a character string specifying the database from which the genome shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"}, \code{db = "emsembl"} or \code{db = "ensemblgenomes"}.
#' @param type type of sequences that shall be retrieved. Either \code{genome}, \code{proteome}, or \code{CDS}.
#' @param path path to the folder in which downloaded genomes shall be stored. By default the
#' kingdom name is used to name the output folder.
#' @author Hajk-Georg Drost
#' @details This function aims to perform bulk retrieval of the genomes of species
#' that belong to the same kingdom of life.
#' @examples 
#' \dontrun{
#' # get all available kingdoms for refseq
#' getKingdoms(db = "refseq")
#' # download all vertebrate genomes from refseq
#' meta.retrieval(kingdom = "vertebrate_mammalian", db = "refseq", type = "genome")
#' 
#' # get all available kingdoms for genbank
#' getKingdoms(db = "genbank")
#' # download all vertebrate genomes from genbank
#' meta.retrieval(kingdom = "vertebrate_mammalian", db = "genbank", type = "genome")
#' }
#' @export

meta.retrieval <- function(kingdom, 
                           db         = "refseq", 
                           type       = "genome", 
                           path = NULL){
        
    subfolders <- getKingdoms(db = db)
    
    if (!is.element(kingdom, subfolders))
        stop(paste0(
            "Please select a valid kingdom: ",
            paste0(subfolders, collapse = ", ")
        ))
    
    if (!is.element(type, c("genome", "proteome", "CDS", "gff")))
        stop("Please choose either type: 'genome', 'proteome', 'CDS', or 'gff'")
    
    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop("Please select einter db = 'refseq' or db = 'genbank'")
    
    if ((type == "CDS") && (db == "genbank"))
        stop("Genbank does not store CDS data. Please choose 'db = 'refseq''.")
    
    
    # getOrganisms <-
    #     try(RCurl::getURL(
    #         paste0("ftp://ftp.ncbi.nlm.nih.gov/genomes/", db, "/", kingdom, "/"),
    #         ftp.use.epsv = FALSE,
    #         dirlistonly = TRUE
    #     ))
    # FilterOrganisms <- strsplit(getOrganisms, "\n")
    # FinalOrganisms <-
    #     stringr::str_replace(unlist(FilterOrganisms), "_", " ")
    # FinalOrganisms <-
    #     FinalOrganisms[-which(is.element(
    #         FinalOrganisms,
    #         c("assembly summary_historical.txt", "assembly summary.txt")
    #     ))]
    
    #organism_name <- NULL
    assembly.summary.file <-
        getSummaryFile(db = db, kingdom = kingdom)
    #assembly.summary.file <-
    #    dplyr::mutate(assembly.summary.file, organism_name = clean.str.brackets(organism_name))
    FinalOrganisms <- unique(assembly.summary.file$organism_name)
    
    cat("\n")
    
    cat(paste0("Starting meta retrieval of all ", type, "s for ", kingdom, "."))
    cat("\n")
    
    if (type == "genome") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getGenome(db       = db,
                          organism = FinalOrganisms[i],
                          path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getGenome(db       = db,
                          organism = FinalOrganisms[i],
                          path     = path)
            }
        }
    }
    
    if (type == "proteome") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getProteome(db       = db,
                            organism = FinalOrganisms[i],
                            path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getProteome(db       = db,
                            organism = FinalOrganisms[i],
                            path     = path)
            }
        }
    }
    
    if (type == "CDS") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getCDS(db       = db,
                       organism = FinalOrganisms[i],
                       path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getCDS(db       = db,
                       organism = FinalOrganisms[i],
                       path     = path)
            }
        }
    }
    
    if (type == "gff") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getGFF(db       = db,
                       organism = FinalOrganisms[i],
                       path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getGFF(db       = db,
                       organism = FinalOrganisms[i],
                       path     = path)
            }
        }
    }
    
    cat("\n")
    cat("Finished meta retieval process.")
}




