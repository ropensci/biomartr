#' @title Perform Meta-Genome Retieval
#' @description Download genomes, proteomes, cds, gff, rna, or assembly stats 
#' files of all species within a kingdom of life.
#' @param kingdom a character string specifying the kingdom of the organisms 
#' of interest, e.g. "archaea","bacteria", "fungi", "invertebrate", "plant", 
#' "protozoa", "vertebrate_mammalian", or "vertebrate_other".
#' Available kingdoms can be retrieved with \code{\link{getKingdoms}}.
#' @param group only species belonging to this subgroup will be downloaded. 
#' Groups can be retrieved with \code{\link{getGroups}}.
#' @param db a character string specifying the database from which the genome 
#' shall be retrieved: \code{db = "refseq"}, \code{db = "genbank"}, 
#' \code{db = "emsembl"} or \code{db = "ensemblgenomes"}.
#' @param type type of sequences that shall be retrieved. Options are:
#' \itemize{
#'  \item \code{type = "genome"} :
#'  (for genome assembly retrieval; see also \code{\link{getGenome}}), 
#'  \item \code{type = "proteome"} :
#'  (for proteome retrieval; see also \code{\link{getProteome}}),
#'  \item \code{type = "CDS"} :
#'  (for coding sequence retrieval; see also \code{\link{getCDS}}),
#'  \item \code{type = "gff"} :
#' (for annotation file retrieval in gff format; see also \code{\link{getGFF}}),
#' \item \code{type = "gtf"} :
#' (for annotation file retrieval in gtf format (only for ensembl and
#'  ensemblgenomes); see also \code{\link{getGTF}})
#'  \item \code{type = "rna"} :
#'  (for RNA file retrieval in fasta format; see also \code{\link{getRNA}}),
#'  \item \code{type = "rm"} :
#'  (for Repeat Masker output file retrieval; see also 
#'  \code{\link{getRepeatMasker}}),
#'  \item \code{type = "assemblystats"} :
#'  (for genome assembly quality stats file retrieval; 
#'  see also \code{\link{getAssemblyStats}}).
#'  }
#' @param combine just in case \code{type = "assemblystats"} is specified, shall
#' assemby stats of individual species be imported and combined to a 
#' \code{\link{data.frame}}? 
#' @param path path to the folder in which downloaded genomes shall be stored. 
#' By default the kingdom name is used to name the output folder.
#' @author Hajk-Georg Drost
#' @details This function aims to perform bulk retrieval of the genomes, 
#' proteomes, cds, etc. of species that belong to the same kingdom of life or 
#' to the same subgroup.
#' @examples 
#' \dontrun{
#' # get all available kingdoms for refseq
#' getKingdoms(db = "refseq")
#' # download all vertebrate genomes from refseq
#' meta.retrieval(kingdom = "vertebrate_mammalian", 
#'                db = "refseq", 
#'                type = "genome")
#' 
#' # get all available kingdoms for genbank
#' getKingdoms(db = "genbank")
#' # download all vertebrate genomes from genbank
#' meta.retrieval(kingdom = "vertebrate_mammalian", 
#'                db = "genbank", 
#'                type = "genome")
#' 
#' # get all available kingdoms for ensemblgenomes
#' getKingdoms(db = "ensemblgenomes")
#' # download all vertebrate genomes from ensemblgenomes
#' meta.retrieval(kingdom = "vertebrate_mammalian", 
#'                db = "ensemblgenomes", 
#'                type = "genome")
#' 
#' # In case users do not wish to retrieve genomes from an entire kingdom, 
#' # but rather from a subgoup (e.g. from species belonging to the 
#' # Gammaproteobacteria class, a subgroup of the bacteria kingdom), 
#' # they can use the following workflow"
#' # First, users can again consult the getKingdoms() function to retrieve 
#' # kingdom information.
#' getKingdoms(db = "refseq")
#' 
#' # In this example, we will choose the bacteria kingdom. 
#' # Now, the getGroups() function allows users to obtain available 
#' # subgroups of the bacteria kingdom.
#' getGroups(db = "refseq", kingdom = "bacteria")
#' 
#' # Now we choose the group Gammaproteobacteria and specify 
#' # the group argument in the meta.retrieval() function
#' meta.retrieval(kingdom = "bacteria", 
#'    roup = "Gammaproteobacteria", 
#'    db = "refseq", 
#'    type = "genome")
#' }
#' @seealso \code{\link{meta.retrieval.all}}
#' @return a character vector storing the file paths of the retrieved files.
#' @export

meta.retrieval <- function(kingdom,
                           group = NULL,
                           db         = "refseq",
                           type       = "genome",
                           combine    = FALSE,
                           path = NULL) {
    # test internet connection
    connected.to.internet()
    
    division <- subgroup <- NULL
    
    subfolders <- getKingdoms(db = db)
    
    if (!is.element(kingdom, subfolders))
        stop(paste0(
            "Please select a valid kingdom: ",
            paste0(subfolders, collapse = ", ")
        ))
    
    if (!is.null(group))
        if (!is.element(group, getGroups(kingdom = kingdom, db = db)))
            stop(
                "Please specify a group that is supported by getGroups(). 
                Your specification '",
                group,
                "' does not exist in getGroups(kingdom = '",
                kingdom,
                "', db = '",
                db,
                "'). Maybe you used a different db argument in getGroups()?",
                call. = FALSE
            )
    
    if (!is.element(type,
                    c("genome", "proteome", "CDS", "gff", 
                      "rna", "assemblystats", "gtf", "rm")))
        stop(
        "Please choose either type: type = 'genome', type = 'proteome', 
        type = 'CDS', type = 'gff', type = 'gtf',
        type = 'rna', type = 'rm', or type = 'assemblystats'.",
            call. = FALSE
        )
    
    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop(
            "Please select einter db = 'refseq', db = 'genbank', 
            db = 'ensembl' or db = 'ensemblgenomes'.",
            call. = FALSE
        )
    
    if ((type == "CDS") && (db == "genbank"))
        stop("Genbank does not store CDS data. Please choose 'db = 'refseq''.",
             call. = FALSE)
    
    if ((type == "gtf") && (is.element(db, c("genbank", "refseq"))))
            stop("GTF files are only available for type = 'ensembl' and type = 'ensemblgebomes'.")
    
    if (type == "assemblystats" &&
        !is.element(db, c("refseq", "genbank")))
        stop(
            "Unfortunately, assembly stats files are only available for 
            db = 'refseq' and db = 'genbank'.",
            call. = FALSE
        )
    
    if (combine && type != "assemblystats")
        stop(
            "Only option type = 'assemblystats' can use combine = TRUE. Please 
            specify: type = 'assemblystats' and combine = TRUE.",
            call. = FALSE
        )
    
    if ((type == "rm") && (!is.element(db, c("refseq", "genbank"))))
        stop("Repeat Masker output files can only be retrieved from 'refseq'",
             " or 'genbank'.")
    
    
    if (is.element(db, c("refseq", "genbank"))) {
        if (is.null(group)) {
            assembly.summary.file <-
                getSummaryFile(db = db, kingdom = kingdom)
            #assembly.summary.file <-
            #    dplyr::mutate(assembly.summary.file, 
            #    organism_name = clean.str.brackets(organism_name))
            FinalOrganisms <-
                unique(assembly.summary.file$organism_name)
            #organism_name <- NULL
            
        }
        if (!is.null(group)) {
            groups.selection <-
                listGroups(kingdom = kingdom,
                           db = db,
                           details = TRUE)
            groups.selection <-
                dplyr::filter(groups.selection, subgroup %in% group)
            FinalOrganisms <- unique(groups.selection$organism_name)
        }
    }
    
    if (db == "ensembl") {
        summary.file <- get.ensembl.info()
        FinalOrganisms <- unique(summary.file$name)
        FinalOrganisms <-
            stringr::str_replace_all(FinalOrganisms, "_", " ")
        stringr::str_sub(FinalOrganisms, 1, 1) <-
            stringr::str_to_upper(stringr::str_sub(FinalOrganisms, 1, 1))
    }
    
    if (db == "ensemblgenomes") {
        summary.file <- get.ensemblgenome.info()
        summary.file <-
            dplyr::filter(summary.file, division == kingdom)
        FinalOrganisms <- unique(summary.file$name)
        FinalOrganisms <-
            stringr::str_replace_all(FinalOrganisms, "_", " ")
        stringr::str_sub(FinalOrganisms, 1, 1) <-
            stringr::str_to_upper(stringr::str_sub(FinalOrganisms, 1, 1))
    }
    
    
    if (is.null(group))
        message(paste0(
            "Starting meta retrieval of all ",
            type,
            " files for ",
            kingdom,
            "."
        ))
    if (!is.null(group))
        message(
            paste0(
                "Starting meta retrieval of all ",
                type,
                " files within kingdom '",
                kingdom,
                "' and subgroup '",
                group,
                "'."
            )
        )
    
    if (!is.null(path)) {
        if (!file.exists(path)) {
            message("Generating folder ", path, " ...")
            dir.create(path, recursive = TRUE)
        }
    }
    
    paths <- vector("character", length(FinalOrganisms))
    
    if (type == "genome") {
        
    
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getGenome(db       = db,
                          organism = FinalOrganisms[i],
                          path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getGenome(db       = db,
                          organism = FinalOrganisms[i],
                          path     = path)
            }
        }
    }
    
    if (type == "proteome") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getProteome(db       = db,
                            organism = FinalOrganisms[i],
                            path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getProteome(db       = db,
                            organism = FinalOrganisms[i],
                            path     = path)
            }
        }
    }
    
    if (type == "CDS") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getCDS(db       = db,
                       organism = FinalOrganisms[i],
                       path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getCDS(db       = db,
                       organism = FinalOrganisms[i],
                       path     = path)
            }
        }
    }
    
    if (type == "gff") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getGFF(db       = db,
                       organism = FinalOrganisms[i],
                       path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getGFF(db       = db,
                       organism = FinalOrganisms[i],
                       path     = path)
            }
        }
    }
    
    if (type == "gtf") {
            if (is.null(path)) {
                    for (i in seq_len(length(FinalOrganisms))) {
                            paths[i] <- getGTF(db       = db,
                                               organism = FinalOrganisms[i],
                                               path     = kingdom)
                    }
            }
            
            if (!is.null(path)) {
                    for (i in seq_len(length(FinalOrganisms))) {
                            paths[i] <- getGTF(db       = db,
                                               organism = FinalOrganisms[i],
                                               path     = path)
                    }
            }
    }
    
    if (type == "rm") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getRepeatMasker(db       = db,
                                   organism = FinalOrganisms[i],
                                   path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getRepeatMasker(db       = db,
                                   organism = FinalOrganisms[i],
                                   path     = path)
            }
        }
    }
    
    if (type == "rna") {
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                paths[i] <- getRNA(db       = db,
                       organism = FinalOrganisms[i],
                       path     = kingdom)
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                getRNA(db       = db,
                       organism = FinalOrganisms[i],
                       path     = path)
            }
        }
    }
    
    if (type == "assemblystats") {
        stats.files <- vector("list", length(FinalOrganisms))
        
        if (is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                if (combine) {
                    stats.files[i] <- list(
                        getAssemblyStats(
                            db       = db,
                            organism = FinalOrganisms[i],
                            path     = kingdom,
                            type     = "import"
                        )
                    )
                } else {
                    paths[i] <- getAssemblyStats(
                        db       = db,
                        organism = FinalOrganisms[i],
                        path     = kingdom
                    )
                }
            }
        }
        
        if (!is.null(path)) {
            for (i in seq_len(length(FinalOrganisms))) {
                if (combine) {
                    stats.files[i] <- list(
                        getAssemblyStats(
                            db       = db,
                            organism = FinalOrganisms[i],
                            path     = path,
                            type     = "import"
                        )
                    )
                } else {
                    paths[i] <- getAssemblyStats(
                        db       = db,
                        organism = FinalOrganisms[i],
                        path     = path
                    )
                }
            }
        }
    }
    
    if (combine) {
        stats.files <- dplyr::bind_rows(stats.files)
        message("Finished meta retieval process.")
        return(stats.files)
    }
    
    message("Finished meta retieval process.")
    return(paths[!is.element(paths, c("FALSE", "Not available"))])
}



