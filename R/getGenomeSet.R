#' @title Genome Retrieval of multiple Species
#' @description Main genome retrieval function for a set of organism of interest.
#' By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the genome of the organisms of interest
#' will be downloaded and stored locally. Genome files can be retrieved from several databases.
#' @param db a character string specifying the database from which the genome
#' shall be retrieved:
#' \itemize{
#' \item \code{db = "refseq"}
#' \item \code{db = "genbank"}
#' \item \code{db = "ensembl"}
#' \item \code{db = "ensemblgenomes"}
#' }
#' @param organisms a character vector storing the names of the organisms than shall be retrieved.
#' There are three available options to characterize an organism:
#' \itemize{
#' \item by \code{scientific name}: e.g. \code{organism = "Homo sapiens"}
#' \item by \code{database specific accession identifier}: e.g. \code{organism = "GCF_000001405.37"} (= NCBI RefSeq identifier for \code{Homo sapiens})
#' \item by \code{taxonomic identifier from NCBI Taxonomy}: e.g. \code{organism = "9606"} (= taxid of \code{Homo sapiens})
#' }
#' @param reference a logical value indicating whether or not a genome shall be downloaded if it isn't marked
#' in the database as either a reference genome or a representative genome.
#' @param clean_retrieval logical value indicating whether or not downloaded files shall be renamed for more convenient downstream data analysis.
#' @param gunzip a logical value indicating whether or not files should be unzipped.
#' @param path a character string specifying the location (a folder) in which
#' the corresponding genomes shall be stored. Default is
#' \code{path} = \code{"set_genomes"}.
#' @author Hajk-Georg Drost
#' @details Internally this function loads the the overview.txt file from NCBI:
#'
#'  refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/
#'
#'  genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/
#'
#' and creates a directory 'set_genomes' to store
#' the genomes of interest as fasta files for future processing.
#' In case the corresponding fasta file already exists within the
#' 'set_genomes' folder and is accessible within the workspace,
#' no download process will be performed.
#' @return File path to downloaded genomes.
#' @examples \dontrun{
#' biomartr::getGenomeSet("refseq", organisms = c("Arabidopsis thaliana", 
#'                                                "Arabidopsis lyrata", 
#'                                                "Capsella rubella"))
#' }
#'
#' @seealso \code{\link{getProteomeSet}}, \code{\link{getCDS}},
#' \code{\link{getGFF}}, \code{\link{getRNA}}, \code{\link{meta.retrieval}},
#' \code{\link{read_genome}}
#' @export

getGenomeSet <-
    function(db = "refseq",
             organisms,
             reference = TRUE,
             clean_retrieval = TRUE,
             gunzip = TRUE,
             path = "set_genomes") {
        message(
            "Starting genome retrieval of the following genomes: ",
            paste0(organisms, collapse = ", "),
            " ..."
        )
        
        if (!file.exists(path)) {
            message("Generating folder ", path, " ...")
            dir.create(path, recursive = TRUE)
        }
        
        if (!file.exists(file.path(path, "documentation")))
            dir.create(file.path(path, "documentation"))
        
        paths <- vector("character", length(organisms))
        
        for (i in seq_len(length(organisms))) {
            paths[i] <- getGenome(db       = db,
                                  organism = organisms[i],
                                  reference = reference,
                                  path     = path)
            message("\n")
        }
        
        meta_files <- list.files(path)
        meta_files <- meta_files[stringr::str_detect(meta_files, "doc_")]
        file.rename(from = file.path(path, meta_files), to = file.path(path, "documentation", meta_files))
        
        doc_tsv_files <- file.path(path,"documentation", meta_files[stringr::str_detect(meta_files, "[.]tsv")])
        
        summary_log <- dplyr::bind_rows(lapply(doc_tsv_files, function(data) {
            suppressMessages(readr::read_tsv(data))
        }))
        
        readr::write_excel_csv(summary_log, file.path(path, "documentation", paste0(basename(path), "_summary.csv")))
        message("A summary file (which can be used as supplementary information file in publications) containig retrieval information for all species has been stored at '",file.path(path, "documentation", paste0(basename(path), "_summary.csv")),"'.")
        
        if (clean_retrieval && gunzip)
            clean.retrieval(paths, gunzip = TRUE)
        
        if (clean_retrieval && !gunzip)
            clean.retrieval(paths, gunzip = FALSE)
        
        return(paths)
        
    }
