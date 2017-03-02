#' @title Import Genome Assembly Stats File
#' @description This function reads an organism specific Genome Assembly Stats file that was retrieved with \code{\link{getAssemblyStats}}.
#' @param file a character string specifying the path to the file storing the Genome Assembly Stats file.
#' @param type either \code{type = "raw"} to import the entire genome assembly stats file or \code{type = "stats"} to import overall statistics including all chromosomes, mitochondria and plastids.
#' @author Hajk-Georg Drost
#' @details This function takes a string specifying the path to the Genome Assembly Stats file
#' of interest (e.g. the path returned by \code{\link{getAssemblyStats}}) and imports it.
#' @seealso \code{\link{getAssemblyStats}}, \code{\link{read_genome}}, \code{\link{read_proteome}}, \code{\link{read_cds}}, \code{\link{read_gff}}
#' @export
read_assemblystats <- function(file, type = "raw") {
    
    if (!is.element(type, c("raw", "stats")))
        stop("Please choose a type that is supported byt this function, e.g. type = 'raw' or type = 'stats'.", call. = FALSE)
    
    unit_name <- molecule_name <- statistic <- NULL
    
    assemblystats_file <- readr::read_delim(
        file,
        delim = "\t",
        col_names = c(
            "unit_name",
            "molecule_name",
            "molecule_type",
            "sequence_type",
            "statistic",
            "value"
        ),
        col_types = readr::cols(
            "unit_name" = readr::col_character(),
            "molecule_name" = readr::col_character(),
            "molecule_type" = readr::col_character(),
            "sequence_type" = readr::col_character(),
            "statistic" = readr::col_character(),
            "value" = readr::col_integer()
        ),
        comment = "#"
    )
    
    if (type == "stats") {
        # select all features (including chromosomes, mitochondria, and plastids)
        assemblystats_file.all.features <- dplyr::filter(assemblystats_file, unit_name == "all", molecule_name == "all")
        
        total.length <- dplyr::filter(assemblystats_file.all.features, statistic == "total-length")
        spanned.gaps <- dplyr::filter(assemblystats_file.all.features, statistic == "spanned-gaps")
        unspanned.gaps <- dplyr::filter(assemblystats_file.all.features, statistic == "unspanned-gaps")
        region.count <- dplyr::filter(assemblystats_file.all.features, statistic == "region-count")
        scaffold.count <- dplyr::filter(assemblystats_file.all.features, statistic == "scaffold-count")
        scaffold.N50 <- dplyr::filter(assemblystats_file.all.features, statistic == "scaffold-N50")
        scaffold.L50 <- dplyr::filter(assemblystats_file.all.features, statistic == "scaffold-L50")
        scaffold.N75 <- dplyr::filter(assemblystats_file.all.features, statistic == "scaffold-N75")
        scaffold.N90 <- dplyr::filter(assemblystats_file.all.features, statistic == "scaffold-N90")
        contig.count <- dplyr::filter(assemblystats_file.all.features, statistic == "contig-count")
        contig.N50 <- dplyr::filter(assemblystats_file.all.features, statistic == "contig-N50")
        total.gap.length <- dplyr::filter(assemblystats_file.all.features, statistic == "total-gap-length")
        molecule.count <- dplyr::filter(assemblystats_file.all.features, statistic == "molecule-count")
        top.level.count <- dplyr::filter(assemblystats_file.all.features, statistic == "top-level-count")
        
        assemblystats_file.all.features.short <-
        tibble::tibble(total_length = ifelse(nrow(total.length) > 0, total.length$value, NA),
                       spanned_gaps = ifelse(nrow(spanned.gaps) > 0, spanned.gaps$value, NA),
                       unspanned_gaps = ifelse(nrow(unspanned.gaps) > 0, unspanned.gaps$value, NA),
                       region_count = ifelse(nrow(region.count) > 0, region.count$value, NA),
                       scaffold_count = ifelse(nrow(scaffold.count) > 0, scaffold.count$value, NA),
                       scaffold_N50 = ifelse(nrow(scaffold.N50) > 0, scaffold.N50$value, NA),
                       scaffold_L50 = ifelse(nrow(scaffold.L50) > 0, scaffold.L50$value, NA),
                       scaffold_N75 = ifelse(nrow(scaffold.N75) > 0, scaffold.N75$value, NA),
                       scaffold_N90 = ifelse(nrow(scaffold.N90) > 0, scaffold.N90$value, NA),
                       contig_count = ifelse(nrow(contig.count) > 0, contig.count$value, NA),
                       contig_N50 = ifelse(nrow(contig.N50) > 0, contig.N50$value, NA),
                       total_gap_length = ifelse(nrow(total.gap.length) > 0, total.gap.length$value, NA),
                       molecule_count = ifelse(nrow(molecule.count) > 0, molecule.count$value, NA),
                       top_level_count = ifelse(nrow(top.level.count) > 0, top.level.count$value, NA)
                       )
        
        return(assemblystats_file.all.features.short)
    }
   
    if (type == "raw") {
        return(assemblystats_file)
    }
    
}

