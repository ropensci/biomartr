getSubgroups <- function(kingdom, db = "refseq") {
    
    if (!is.element(kingdom, getKingdoms(db = db)))
        stop(paste0(
            "Please select a valid kingdom: ",
            paste0(getKingdoms(), collapse = ", ")
        ), call. = FALSE)
    
    if (!is.element(db, c("refseq", "genbank","ensemblgenomes")))
        stop("Please select one of the available data bases: 'refseq', 'genbank', 'ensemblgenomes'", call. = FALSE)
    
    # get Kingdom Assembly Summary file
    AssemblyFilesAllKingdoms <- getKingdomAssemblySummary(db = db)
    
    # get all genomes list
    all.genomes <- listGenomes("all", TRUE, db = "all")
    
    # join tables
    joined.df <- dplyr::inner_join(AssemblyFilesAllKingdoms,all.genomes, by = "organism_name")
    
    return(names(table(joined.df$subgroups)))
}
