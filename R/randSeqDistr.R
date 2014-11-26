#' @title Function to generate a distribution of alignment scores based on random sequences
#' @description This function takes a sequence, a vector of randomly sampled sequences, and a
#' alignment function as input and computes an pairwise alignment score distribution from the input sequence
#' and all random sequences stored in \code{sampled_strings}.
#' @param seq
#' @param sampled_strings
#' @param FUN
#' @author Hajk-Georg Drost
#' @return a numeric vector storing pairwise alignment scores.
#' @examples 
#' 
#' # pairwise alignments using Biostrings 
#' 
#' @export  

randSeqDistr <- function(seq, sampled_strings, FUN, ...){
        
        align.fun <- match.fun(FUN)
        n_samples <- length(sampled_strings) 
        res <- vector(mode = "list", length = n_samples)
        res <- sapply(1:n_samples, function(x) align.fun(seq , sampled_strings[x], ...))

        return(res)
}
