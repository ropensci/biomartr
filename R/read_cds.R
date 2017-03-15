#' @title Import CDS as Biostrings or data.table object
#' @description This function reads an organism specific CDS stored in a defined file format.
#' @param file a character string specifying the path to the file storing the CDS.
#' @param format a character string specifying the file format used to store the genome, e.g. \code{format = "fasta"} (default) or \code{format = "gbk"}.
#' @param obj.type a character string specifying the object stype in which the genomic sequence shall be represented. 
#' Either as \code{obj.type = "Biostrings"} (default) or as \code{obj.type = "data.table"}.
#' @param delete_corrupt a logical value specifying whether potential CDS sequences that cannot be divided by 3 shall be
#' be excluded from the the dataset. Default is \code{delete_corrupt = FALSE}.
#' @param ... additional arguments that are used by the seqinr::read.fasta() function.
#' @author Hajk-Georg Drost
#' @details The \code{read.cds} function takes a string specifying the path to the cds file
#' of interest as first argument.
#'
#' It is possible to read in different proteome file standards such as \emph{fasta} or \emph{genebank}.
#'
#' CDS stored in fasta files can be downloaded from http://www.ensembl.org/info/data/ftp/index.html.
#'
#' @examples \dontrun{
#' # reading a cds file stored in fasta format
#' Ath.cds <- read_cds(system.file('seqs/ortho_thal_cds.fasta', package = 'orthologr'),
#'                     format = "fasta")
#' }
#'
#' @return A data.table storing the gene id in the first column and the corresponding
#' sequence as string in the second column.
#' @seealso \code{\link{getCDS}}, \code{\link{read_genome}}, \code{\link{read_proteome}}, \code{\link{read_gff}}, \code{\link{read_rna}}
#' @import Biostrings
#' @export
read_cds <- function(file, format = "fasta", obj.type = "Biostrings", delete_corrupt = FALSE, ...){
    
    if (!is.element(format, c("fasta", "gbk")))
        stop("Please choose a file format that is supported by this function.", call. = FALSE)
    
    if (!is.element(obj.type, c("Biostrings", "data.table")))
        stop(
            "Please specify a valid object type: obj.type = 'Biostrings' (default) or obj.type = 'data.table'.", call. = FALSE
        )
    
    geneids <- seqs <- NULL
    
    if (obj.type == "Biostrings") { 
        tryCatch({
            cds_file <-
                Biostrings::readBStringSet(filepath = file, format = format, ...)}, error = function(e) {
                    stop(
                        paste0(
                            "File ",
                            file,
                            " could not be read properly. \n",
                            "Please make sure that ",
                            file,
                            " contains only CDS sequences and is in ",
                            format,
                            " format."
                        ), call. = FALSE
                    )
                })
        
        return(cds_file)
    }
    
    if (obj.type == "data.table") {
        tryCatch({
            cds_file <-
                Biostrings::readBStringSet(filepath = file, format = format, ...)
            
            cds_names <-
                as.vector(unlist(sapply(cds_file@ranges@NAMES, function(x) {
                    return(strsplit(x, " ")[[1]][1])
                })))
            
            cds.dt <-
                data.table::data.table(geneids = cds_names ,
                                       seqs = tolower(as.character(cds_file)))
            
            
            data.table::setkey(cds.dt, geneids)
            
            mod3 <-
                function(x) {
                    return((nchar(x) %% 3) == 0)
                }
            
            all_triplets <- cds.dt[, mod3(seqs)]
            
            if (!all(all_triplets)) {
                warning(
                    paste0(
                        "There have been ",
                        length(which(!all_triplets)),
                        " genes found that cannot be divided by 3. To delete these sequences please specify the 'delete_corrupt = TRUE' argument."
                    )
                )
            }
        }, error = function(e) {
            stop(
                paste0(
                    "File ",
                    file,
                    " could not be read properly. \n",
                    "Please make sure that ",
                    file,
                    " contains only CDS sequences and is in ",
                    format,
                    " format."
                ), call. = FALSE
            )
        })
        
        if (!all(all_triplets)) {
            if (delete_corrupt)
                return(cds.dt[-which(!all_triplets) , list(geneids, seqs)])
            
            if (!delete_corrupt)
                return(cds.dt)
            
        } else {
            return(cds.dt)
        }
    }
}
