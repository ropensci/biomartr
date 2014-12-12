#' @title Retrieve biological sequences of a given set of genes
#' @description This function takes an character vector storing gene ids of interest
#' and returns the biological sequence of the corresponding gene ids.
#' @param genes a character vector storing the gene id of a organisms of interest to be queried against the 
#' acnucdb database.
#' @param db
#' @author Hajk-Georg Drost
#' @details Sequence information is retrieved from the acnucdb database.
#' @return A list of sequences for each corresponding input gene stored as string.
#' @examples # retrieve amino acid sequences from the 'swissprot' database 
#' # for gene ids:"AT1G06090" = "Q9LND9" and "AT1G06100" = "Q9LND8"
#' 
#'  seqs <- geneSequence(c("Q9LND9","Q9LND8"), db = "swissprot")
#'    
#' # choose different databases available
#' 
#'    choosebank()
#'       
#' @export
geneSequence <- function(genes, db){
        
        n_genes <- length(genes)
        
        seqList <- vector(mode = "list", length = n_genes)        
        
        # open acnucdb connection: seqinr
        seqinr::choosebank(db)
        
        # retrieve sequences for the corresponding gene list
        seqList <- lapply(as.list(genes), retrieve_sequence)
        
        # close acnucdb connection: seqinr
        seqinr::closebank()
        
        # return sequences as strings
        res <- vector(mode = "list", length = n_genes)
        res <- lapply(seqList, seqinr::c2s)
        names(res) <- genes
        
        return(res)
        
}

