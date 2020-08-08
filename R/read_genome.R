#' @title Import Genome Assembly as Biostrings or data.table object
#' @description This function reads an organism specific genome stored in a 
#' defined file format.
#' @param file a character string specifying the path to the file 
#' storing the genome.
#' @param format a character string specifying the file format used to store 
#' the genome, e.g. \code{format = "fasta"} (default) or \code{format = "gbk"}.
#' @param obj.type a character string specifying the object stype in which 
#' the genomic sequence shall be represented. Either as 
#' \code{obj.type = "Biostrings"} (default) or 
#' as \code{obj.type = "data.table"}.
#' @param ... additional arguments that are used by the 
#' \code{\link[seqinr]{read.fasta}} function.
#' @author Hajk-Georg Drost
#' @details This function takes a string specifying the path to the genome file
#' of interest as first argument (e.g. the path returned by 
#' \code{\link{getGenome}}).
#' @return Either a \code{Biostrings} or \code{data.table} object.
#' @seealso \code{\link{getGenome}}, \code{\link{read_proteome}}, 
#' \code{\link{read_cds}}, \code{\link{read_gff}}, \code{\link{read_rna}}
#' @import Biostrings
#' @export

read_genome <-
    function(file,
             format = "fasta",
             obj.type = "Biostrings",
             ...) {
        if (!is.element(format, c("fasta", "gbk")))
            stop("Please choose a file format that is 
                 supported by this function.",
                 call. = FALSE)
        
        if (!file.exists(file))
            stop("The file path you specified does not seem to exist: '", file,"'.", call. = FALSE)
        
        if (!is.element(obj.type, c("Biostrings", "data.table")))
            stop(
                "Please specify a valid object type: obj.type = 'Biostrings' 
                (default) or obj.type = 'data.table'.",
                call. = FALSE
            )
        geneids <- NULL
        
        if (obj.type == "Biostrings") {
            tryCatch({
                genome <-
                    Biostrings::readDNAStringSet(filepath = file, 
                                                 format = format, ...)
            }, error = function(e) {
                stop(
                    paste0(
                        "File ",
                        file,
                        " could not be read properly. \n",
                        "Please make sure that ",
                        file,
                        " contains only DNA sequences and is in ",
                        format,
                        " format."
                    ),
                    call. = FALSE
                )
            })
            
            return(genome)
        }
        
        if (obj.type == "data.table") {
            tryCatch({
                genome <-
                    Biostrings::readDNAStringSet(filepath = file, 
                                                 format = format, ...)
                genome_names <-
                    as.vector(unlist(lapply(genome@ranges@NAMES, function(x) {
                        return(strsplit(x, " ")[[1]][1])
                    })))
                genome.dt <-
                    data.table::data.table(geneids = genome_names,
                                           seqs = tolower(as.character(genome)))
                
                data.table::setkey(genome.dt, geneids)
                
            }, error = function(e) {
                stop(
                    paste0(
                        "File ",
                        file,
                        " could not be read properly. \n",
                        "Please make sure that ",
                        file,
                        " contains only DNA sequences and is in ",
                        format,
                        " format."
                    ),
                    call. = FALSE
                )
            })
            
            return(genome.dt)
        }
    }
