#' @title Import RNA as Biostrings or data.table object
#' @description This function reads an organism specific RNA stored in a 
#' defined file format.
#' @param file a character string specifying the path to the file 
#' storing the RNA.
#' @param format a character string specifying the file format used to store the
#'  genome, e.g. \code{format = "fasta"} (default) or \code{format = "gbk"}.
#' @param obj.type a character string specifying the object stype in which the 
#' genomic sequence shall be represented. 
#' Either as \code{obj.type = "Biostrings"} (default) or 
#' as \code{obj.type = "data.table"}.
#' @param ... additional arguments that are used by 
#' \code{\link[seqinr]{read.fasta}}.
#' @author Hajk-Georg Drost
#' @details This function takes a string specifying the path to the RNA file
#' of interest as first argument. It is possible to read in different proteome 
#' file standards such as \emph{fasta} or \emph{genebank}.
#' @return A data.table storing the gene id in the first column and the 
#' corresponding sequence as string in the second column.
#' @seealso \code{\link{getRNA}}, \code{\link{read_genome}}, 
#' \code{\link{read_proteome}}, \code{\link{read_gff}}
#' @import Biostrings
#' @export

read_rna <-
    function(file,
             format = "fasta",
             obj.type = "Biostrings",
             ...) {
        if (!is.element(format, c("fasta", "gbk")))
            stop("Please choose a file format that is supported 
                 by this function.",
                 call. = FALSE)
        
        if (!is.element(obj.type, c("Biostrings", "data.table")))
            stop(
                "Please specify a valid object type: 
                obj.type = 'Biostrings' (default) or obj.type = 'data.table'.",
                call. = FALSE
            )
        
        geneids <- NULL
        
        if (obj.type == "Biostrings") {
            tryCatch({
                RNA_file <-
                    Biostrings::readRNAStringSet(filepath = file, 
                                                 format = format, ...)
            }, error = function(e) {
                stop(
                    paste0(
                        "File ",
                        file,
                        " could not be read properly. \n",
                        "Please make sure that ",
                        file,
                        " contains only RNA sequences and is in ",
                        format,
                        " format."
                    ),
                    call. = FALSE
                )
            })
            
            return(RNA_file)
        }
        
        if (obj.type == "data.table") {
            tryCatch({
                RNA_file <-
                    Biostrings::readRNAStringSet(filepath = file, 
                                                 format = format, ...)
                
                RNA_names <-
                    as.vector(unlist(lapply(RNA_file@ranges@NAMES, function(x) {
                        return(strsplit(x, " ")[[1]][1])
                    })))
                
                RNA.dt <-
                    data.table::data.table(geneids = RNA_names ,
                                           seqs = 
                                               tolower(as.character(RNA_file)))
                
                
                data.table::setkey(RNA.dt, geneids)
                
    
            }, error = function(e) {
                stop(
                    paste0(
                        "File ",
                        file,
                        " could not be read properly. \n",
                        "Please make sure that ",
                        file,
                        " contains only RNA sequences and is in ",
                        format,
                        " format."
                    ),
                    call. = FALSE
                )
            })
                return(RNA.dt)
        }
    }
