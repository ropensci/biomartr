#' @title Import Proteome as Biostrings or data.table object
#' @description This function reads an organism specific proteome stored in a defined file format.
#' @param file a character string specifying the path to the file storing the proteome.
#' @param format a character string specifying the file format used to store the genome, e.g. \code{format = "fasta"} (default) or \code{format = "gbk"}.
#' @param obj.type a character string specifying the object stype in which the genomic sequence shall be represented. 
#' Either as \code{obj.type = "Biostrings"} (default) or as \code{obj.type = "data.table"}.
#' @param ... additional arguments that are used by the \code{seqinr::read.fasta()} function.
#' @author Hajk-Georg Drost
#' @details This function takes a string specifying the path to the proteome file
#' of interest as first argument.
#'
#' It is possible to read in different proteome file standards such as \emph{fasta} or \emph{genebank}.
#'
#'
#' @examples \dontrun{
#' # import a proteome stored as fasta
#' Ath.proteome <- read_proteome(system.file('seqs/ortho_thal_aa.fasta', package = 'orthologr'),
#'                                format = "fasta",
#'                                obj.type = "Biostrings")
#'          
#' Ath.proteome
#' }
#'
#' @return Either a \code{Biostrings} or \code{data.table} object.
#' @seealso \code{\link{getProteome}}, \code{\link{read_genome}}, \code{\link{read_gff}}, \code{\link{read_cds}}
#' @import Biostrings
#' @export

read_proteome <- function(file, format = "fasta", obj.type = "Biostrings", ...){
    
    if (!is.element(format, c("fasta", "gbk")))
        stop("Please choose a file format that is supported by this function.",
             call. = FALSE)
    
    if (!is.element(obj.type, c("Biostrings", "data.table")))
        stop(
            "Please specify a valid object type: obj.type = 'Biostrings' (default) or obj.type = 'data.table'.",
            call. = FALSE
        )
    
    geneids <- NULL
    
    if (obj.type == "Biostrings") {
        tryCatch({
            proteome <-
                Biostrings::readAAStringSet(filepath = file, format = format, ...)
        }, error = function(e) {
            stop(
                paste0(
                    "File ",
                    file,
                    " could not be read properly. \n",
                    "Please make sure that ",
                    file,
                    " contains only amino acid sequences and is in ",
                    format,
                    " format."
                ),
                call. = FALSE
            )
        })
        
        return(proteome)
    }
    
    if (obj.type == "data.table") {
        tryCatch({
            proteome <-
                Biostrings::readAAStringSet(filepath = file, format = format, ...)
            proteome_names <-
                as.vector(unlist(sapply(proteome@ranges@NAMES, function(x) {
                    return(strsplit(x, " ")[[1]][1])
                })))
            proteome.dt <-
                data.table::data.table(geneids = proteome_names,
                                       seqs = tolower(as.character(proteome)))
            
            data.table::setkey(proteome.dt, geneids)
            
        }, error = function(e) {
            stop(
                paste0(
                    "File ",
                    file,
                    " could not be read properly. \n",
                    "Please make sure that ",
                    file,
                    " contains only amino acid sequences and is in ",
                    format,
                    " format."
                ),
                call. = FALSE
            )
        })
        
        return(proteome.dt)
    }
}
