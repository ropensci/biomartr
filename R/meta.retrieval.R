#' @title Perform Meta-Genome Retieval
#' @description Download genomes, proteomes, or CDS of all species within a kingdom of life.
#' @param kingdom a character string specifying the kingdom of the organisms of interest,
#' e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", "protozoa", "vertebrate_mammalian", or "vertebrate_other".
#' Available kingdoms can be retrieved with \code{\link{getKingdoms}}.
#' @param group only species belonging to this subgroup will be downloaded. Groups can be retrieved with \code{\link{getGroups}}.
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
#' 
#' # get all available kingdoms for ensemblgenomes
#' getKingdoms(db = "ensemblgenomes")
#' # download all vertebrate genomes from ensemblgenomes
#' meta.retrieval(kingdom = "", db = "ensemblgenomes", type = "genome")
#' 
#' }
#' @export

meta.retrieval <- function(kingdom,
                           group = NULL,
                           db         = "refseq", 
                           type       = "genome", 
                           path = NULL){
    
    connected.to.internet()
    
    division <- NULL
    
    subfolders <- getKingdoms(db = db)
    
    if (!is.element(kingdom, subfolders))
        stop(paste0(
            "Please select a valid kingdom: ",
            paste0(subfolders, collapse = ", ")
        ))
    
    if (!is.null(group))
        if (!is.element(group, getGroups(kingdom = kingdom, db = db)))
            stop("Please specify a group that is supported by getGroups(). Your specification '",group,"' does not exist in getGroups(kingdom = '",kingdom,"', db = '",db,"'). Maybe you used a different db argument in getGroups()?", call. = FALSE)
    
    if (!is.element(type, c("genome", "proteome", "CDS", "gff")))
        stop("Please choose either type: 'genome', 'proteome', 'CDS', or 'gff'")
    
    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop("Please select einter db = 'refseq', db = 'genbank', db = 'ensembl' or db = 'ensemblgenomes'.")
    
    if ((type == "CDS") && (db == "genbank"))
        stop("Genbank does not store CDS data. Please choose 'db = 'refseq''.")
    
    if (is.element(db, c("refseq", "genbank"))) {
        
        if (is.null(group)) {
            assembly.summary.file <-
                getSummaryFile(db = db, kingdom = kingdom)
            #assembly.summary.file <-
            #    dplyr::mutate(assembly.summary.file, organism_name = clean.str.brackets(organism_name))
            FinalOrganisms <- unique(assembly.summary.file$organism_name)
            #organism_name <- NULL
            
        }
        if (!is.null(group)) {
            groups.selection <- listGroups(kingdom = kingdom, db = db, details = TRUE)
            groups.selection <- dplyr::filter(groups.selection, subgroup %in% group)
            FinalOrganisms <- unique(groups.selection$organism_name)
        }
    }
    
    if (db == "ensembl") {
        summary.file <- get.ensembl.info()
        FinalOrganisms <- unique(summary.file$name)
        FinalOrganisms <- stringr::str_replace_all(FinalOrganisms,"_"," ")
        stringr::str_sub(FinalOrganisms,1,1) <- stringr::str_to_upper(stringr::str_sub(FinalOrganisms,1,1))
    }
    
    if (db == "ensemblgenomes") {
        summary.file <- get.ensemblgenome.info()
        summary.file <- dplyr::filter(summary.file, division == kingdom)
        FinalOrganisms <- unique(summary.file$name)
        FinalOrganisms <- stringr::str_replace_all(FinalOrganisms,"_"," ")
        stringr::str_sub(FinalOrganisms,1,1) <- stringr::str_to_upper(stringr::str_sub(FinalOrganisms,1,1))
    }
    
    cat("\n")
    
    if (is.null(group))
        cat(paste0("Starting meta retrieval of all ", type, "s for ", kingdom, "."))
    if (!is.null(group))
        cat(paste0("Starting meta retrieval of all ", type, "s within kingdom '", kingdom, "' and subgroup '",group,"'."))
    
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




