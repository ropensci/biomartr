#' @title Import GFF File
#' @description This function reads an organism specific CDS stored 
#' in a defined file format.
#' @param file a character string specifying the path to the file 
#' storing the CDS.
#' @author Hajk-Georg Drost
#' @details This function takes a string specifying the path to the GFF file
#' of interest (e.g. the path returned by \code{\link{getGFF}}).
#' @return Either a \code{Biostrings} or \code{data.table} object.
#' @seealso \code{\link{getGenome}}, \code{\link{read_genome}}, 
#' \code{\link{read_proteome}}, \code{\link{read_cds}}, \code{\link{read_rna}}
#' @export

read_gff <- function(file) {
    # read gtf file content
    suppressWarnings(
        gff.input <-
            readr::read_delim(
                file = file,
                delim = "\t",
                col_names = FALSE,
                comment = "#"
            )
    )
    
    if (!file.exists(file))
        stop("The file path you specified does not seem to exist: '", file,"'.", call. = FALSE)
    
    if (ncol(gff.input) > 9)
        stop("The gff file format can not store more than 9 columns!", call. = FALSE)
    
    
    # name standardized columns
    gffNames <- c("seqid",
                  "source",
                  "type",
                  "start",
                  "end",
                  "score",
                  "strand",
                  "phase",
                  "attribute")
    
    names(gff.input)[seq_len(ncol(gff.input))] <-
        gffNames[seq_len(ncol(gff.input))]
    
    return(gff.input)
}


