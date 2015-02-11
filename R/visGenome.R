#' @title Visualize Genome Properties
#' @description This function allows to visualize selected properties of input genomes such as:
#' \itemize{
#' \item gene length distribution: \code{type} = \code{"length_distr"}
#' 
#' }
#' @param data a data.table object storing the corresponding genome as returned by \code{\link{read_genome}}, \code{\link{read_proteome}}, or 
#' \code{\link{read_cds}}.
#' @param type a character string specifying the genome feature that shall be visualized: "length_distr", ... .
#' @param ... additional parameters that shall be passed to the corresponding plot function.
#' @author Hajk-Georg Drost
#' @return a plot object.
#' @examples \dontrun{
#' 
#' # download the proteome of Arabidopsis thaliana from refseq
#' # and store the corresponding proteome file in '_ncbi_downloads/proteomes'
#' 
#' getProteome( db       = "refseq", 
#'              kingdom  = "plant",
#'              organism = "Arabidopsis thaliana" )
#' 
#' # visualize the length distribution of the proteome of A. thaliana
#' visGenome(data = Ath_proteome, type = "length_distr")
#' 
#' }
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


