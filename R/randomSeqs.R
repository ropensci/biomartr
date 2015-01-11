#' @title Random Sequence Generator Based on a Multinomial Model
#' @description This function computes random sequences based on the alphabet and
#' word length of an input sequence based on a multinomial model.
#' @param seq a character vector storing a sequence as string for which random sequences shall be computed.
#' @param sample_size a numeric value specifying the number of random sequences that shall be returned. 
#' @details This function enables you to create a test statistic for sequence comparisons that are based
#' on a multinomial model assumption. When spexifying the \code{sample_size} argument
#' a vector of strings of corresponding \code{sample_size} is being returned.
#' 
#' The random strings returned by \code{randomSeqs} have the same word length and are drawn from
#' the same alphabet as the input sequence.
#' 
#' @author Hajk-Georg Drost
#' @return a character vector of length \code{sample_size} storing the random string objects.
#' @examples
#' 
#' # a nucleotide example
#' seq_example <- "ACCTGGAATTC"
#' 
#' randomSeqs(seq = seq_example, sample_size = 10)
#' 
#' # a protein example
#' seq_example <- "NPPAAM"
#' 
#' randomSeqs(seq = seq_example, sample_size = 10)
#' 
#' @seealso \code{\link{evalAlignment}}, \code{\link{randSeqDistr}}
#' @export 
randomSeqs <- function(seq, sample_size){
        
        
        if(!is.character(seq))
                stop("Please insert a string as seq argument.")
        
        # get sequence length
        l <- nchar(seq)
        
        if(length(seq) > 1)
                stop("Please insert a string as seq argument.")
        
        if(l == 1)
                warning("Are you sure that you want to randomly sample one letter?")
        
        char_seq <- vector(mode = "character", length = l)
        char_seq <- seqinr::s2c(seq)
        
        # count letter frequency
        letter_freq <- table(char_seq) 
        
        # get the alphabet
        alphabet <- names(letter_freq)
        # get alphabet length
        A <- length(alphabet)
        
        # define a probability vector
        prob <- vector(mode = "numeric", length = A)
        
        # compute the relative frequency of letters
        prob <- letter_freq / sum(letter_freq)
        
        randSeqs <- vector(mode = "character", length = sample_size)
        randSeqs <- sapply(1:sample_size, function(x) seqinr::c2s(sample(alphabet, l, rep = TRUE, prob = prob)))
        
        return(randSeqs)
}





