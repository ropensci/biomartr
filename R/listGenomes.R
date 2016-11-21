#' @title List All Available Genomes
#' @description This function retrieves the names of all genomes available on the NCBI ftp:// server and stores
#' the results in a file named 'overview.txt' inside the directory '_ncbi_downloads' that
#' is built inside the workspace.
#' @param db a character string specifying the database for which genome availability shall be checked, 
#' e.g. \code{db = "refseq"}, \code{db = "genbank"}, \code{db = "ensembl"}, \code{db = "ensemblgenomes"}. 
#' @param type a character string specifying a potential filter of available genomes. Options are \code{type = "all"}, \code{type = "kingdom"}, \code{type = "group"}, \code{type = "subgroup"}.
#' @param subset a character string or character vector specifying a subset of \code{type}. E.g. if users are interested in retrieving all
#' \code{Eukaryota} species, they can specify: \code{type = "kingdom"} and \code{subset = "Eukaryota"}. 
#' @param details a boolean value specifying whether only the scientific names of stored genomes shall be returned
#' (details = FALSE) or all information such as \code{organism_name},\code{kingdoms}, \code{group}, \code{subgroup}, \code{file_size_MB}, etc.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#' \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/} and creates a directory '_ncbi_downloads' in the \code{temdir()}
#' folder to store the overview.txt file for future processing. In case the overview.txt file already exists within the
#' '_ncbi_downloads' folder and is accessible within the workspace, no download process will be performed again.
#' @note
#' 
#' Please note that the ftp:// connection relies on the NCBI or ENSEMBL server and cannot be
#' accurately accessed via a proxy. 
#' 
#' @examples 
#' 
#' \dontrun{
#' # print details for refseq
#' listGenomes(db = "refseq") 
#'
#' }
#' 
#' @export

listGenomes <- function(db = "refseq", type = "all", subset = NULL, details = FALSE){
        
    if (!is.element(db, c("refseq", "genbank", "ensembl", "ensemblgenomes")))
        stop(
            "Please specify a database that is supported by this function. E.g. 'refseq', 'genbank', 'ensembl' or 'ensemblgenomes'.",
            call. = FALSE
        )
    
    if (!is.element(type, c("all", "kingdom", "group", "subgroup")))
        stop(
            "Please specify a type that is supported by this function. E.g. 'all', kingdom', 'group', or 'subgroup'.",
            call. = FALSE
        )
    
    subgroup <- division <- NULL
    
    if (is.element(db, c("refseq", "genbank"))) {
        # retrieve genome report overview file
        ncbi_overview <- getGENOMEREPORT()
        
        # get Kingdom Assembly Summary file
        AssemblyFilesAllKingdoms <-
            getKingdomAssemblySummary(db = db)
        
        # join tables to retrieve kingdom, group, subgroup information for refseq/genbank organisms
        joined.df <-
            dplyr::inner_join(AssemblyFilesAllKingdoms, ncbi_overview, by = "organism_name")
        
        kingdoms <- group <-  NULL
        
        if (type == "all") {
            if (details) {
                if (!is.null(subset)) {
                    warning(
                        "For option type = 'all' no subset can be specified. Please select another type and then specify subset = '",
                        subset,
                        "'.",
                        call. = FALSE
                    )
                }
                
                return(joined.df)
            }
            
            if (!details) {
                if (!is.null(subset)) {
                    warning(
                        "For option type = 'all' no subset can be specified. Please select another type and then specify subset = '",
                        subset,
                        "'.",
                        call. = FALSE
                    )
                }
                
                return(unique(joined.df$organism_name))
            }
        }
        
        if (type == "kingdom") {
            if (details) {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(joined.df$kingdoms)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'Eukaryota' instead of 'eukaryota'.",
                            call. = FALSE
                        )
                    
                    return(dplyr::filter(joined.df, is.element(kingdoms, subset)))
                } else {
                    return(joined.df)
                }
            }
            
            if (!details) {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(joined.df$kingdoms)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'Eukaryota' instead of 'eukaryota'.",
                            call. = FALSE
                        )
                    return(unique(
                        dplyr::filter(joined.df, is.element(kingdoms, subset))$organism_name
                    ))
                } else {
                    return(unique(joined.df$organism_name))
                }
            }
        }
        
        if (type == "group") {
            if (details) {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(table(
                        joined.df$group
                    )))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'Eukaryota' instead of 'eukaryota'.",
                            call. = FALSE
                        )
                    return(dplyr::filter(joined.df, is.element(group, subset)))
                } else {
                    return(joined.df)
                }
            }
            
            if (!details) {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(table(
                        joined.df$group
                    )))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'Eukaryota' instead of 'eukaryota'.",
                            call. = FALSE
                        )
                    return(unique(
                        dplyr::filter(joined.df, is.element(group, subset))$organism_name
                    ))
                } else {
                    return(unique(joined.df$organism_name))
                }
            }
        }
        
        if (type == "subgroup") {
            if (details) {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(joined.df$subgroup)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'Eukaryota' instead of 'eukaryota'.",
                            call. = FALSE
                        )
                    return(dplyr::filter(joined.df, is.element(subgroup, subset)))
                } else {
                    return(joined.df)
                }
            }
            
            if (!details) {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(joined.df$subgroup)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'Eukaryota' instead of 'eukaryota'.",
                            call. = FALSE
                        )
                    return(unique(
                        dplyr::filter(joined.df, is.element(subgroup, subset))$organism_name
                    ))
                } else {
                    return(unique(joined.df$organism_name))
                }
            }
        }
    }
    
    if (db == "ensembl") {
        if (!is.element(type, c("all", "kingdom")))
            stop(
                "Unfortunately, ENSEMBL only provides kingdom information and no group or subgroup information."
            )
        
        ensemblinfo <- get.ensembl.info()

        if (details) {
            if (type == "all")
                return(ensemblinfo)
            if (type == "kingdom") {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(ensemblinfo$division)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'EnsemblMetazoa' instead of 'ensemblmetazoa'.",
                            call. = FALSE
                        )
                    return(dplyr::filter(ensemblinfo, is.element(division, subset)))
                    
                } else {
                    return(ensemblinfo)
                }
            }
        }
        
        if (!details) {
            if (type == "all")
                return(unique(ensemblinfo$name))
            if (type == "kingdom") {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(ensemblinfo$division)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'EnsemblMetazoa' instead of 'ensemblmetazoa'.",
                            call. = FALSE
                        )
                    return(unique(dplyr::filter(
                        ensemblinfo, is.element(division, subset)
                    )$name))
                    
                } else {
                    return(unique(ensemblinfo$name))
                }
            }
        }
    }
    
    if (db == "ensemblgenomes") {
        
        if (!is.element(type, c("all", "kingdom")))
            stop(
                "Unfortunately, ENSEMBLGENOMES only provides kingdom information and no group or subgroup information."
            )
        
        ensemblgenomesinfo <-  get.ensemblgenome.info()
       
        if (details) {
            if (type == "all")
                return(ensemblgenomesinfo)
            if (type == "kingdom") {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(ensemblgenomesinfo$division)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'EnsemblMetazoa' instead of 'ensemblmetazoa'.",
                            call. = FALSE
                        )
                    return(dplyr::filter(ensemblgenomesinfo, is.element(division, subset)))
                    
                } else {
                    return(ensemblgenomesinfo)
                }
            }
        }
        
        if (!details) {
            if (type == "all")
                return(unique(ensemblgenomesinfo$name))
            if (type == "kingdom") {
                if (!is.null(subset)) {
                    if (!all(is.element(subset, names(
                        table(ensemblgenomesinfo$division)
                    ))))
                        stop(
                            "Unfortunately, not all memebrs of your specified subset '",
                            paste0(subset, ", '"),
                            " could be found. Search terms are case sensitive, so you could try to type 'EnsemblMetazoa' instead of 'ensemblmetazoa'.",
                            call. = FALSE
                        )
                    return(unique(dplyr::filter(
                        ensemblgenomesinfo, is.element(division, subset)
                    )$name))
                    
                } else {
                    return(unique(ensemblgenomesinfo$name))
                }
            }
        }        
    }
}
        
        
    
    
    
    
    
    

