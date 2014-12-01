#' @title Visualization function for genome properties
#' @description This function allows to visualize selected properties of input genomes such as:
#' \itemize{
#' \item gene length distribution
#' 
#' }
#' @param data a data.table object storing the corresponding genome as returned by \code{\link{read.genome}}, \code{\link{read.proteome}}, or 
#' \code{\link{read.cds}}.
#' @param type a character string specifying the genome feature that shall be visualized: "length_distr", ... .
#' @param ... additional parameters that shall be passed to the corresponding plot function.
#' @author Hajk-Georg Drost
#' @return a plot object.
#' @import data.table
#' @export

visGenome <- function(data, type = "length_distr", ...){
        
        if(!is.element(type,c("length_distr")))
                stop(paste0("type = ",type," is not supported by this function."))
        
        seqs <- NULL
        
        if(type == "length_distr"){
                
                data_dt <- data.table::copy(data)
                hist(data_dt[ , nchar(seqs)],  ...)
                rug(data_dt[ , nchar(seqs)])
        }
        
        
}


