#' @title Generate a Distribution of Alignment Scores Based on Random Sequences
#' @description This function takes a sequence, a vector of randomly sampled sequences, and a
#' alignment function as input and computes an pairwise alignment score distribution from the input sequence
#' and all random sequences stored in \code{sampled_strings}.
#' @param seq a character vector storing a sequence as string for which random sequences shall be computed.
#' @param subject a character vector storing a subject sequence as string to which \code{seq} shall be pairwise aligned.
#' @param sampled_strings an vector object returned by the \code{\link{randomSeqs}} function.
#' @param FUN a pairwise alignment function such as \code{\link[Biostrings]{pairwiseAlignment}} or any other function
#' that takes sequence arguments as first and second input.
#' @param ... additional arguments that shall be passed to \code{FUN}.
#' @param comp_cores a numeric value specifying the number of cores you want to use for multicore processing.
#' @author Hajk-Georg Drost
#' @return a numeric vector storing pairwise alignment scores.
#' @examples 
#' 
#' # pairwise alignments using Biostrings::pairwiseAlignment() function
#' # you can also specify the comp_cores argument in case you 
#' # are working with a multicore machine 
#' 
#' seq_example <- "MEDQVGFGF"
#' subject_example <- "AYAIDPTPAF"
#' 
#' randScores <- randSeqDistr(seq_example,subject_example,
#'                            randomSeqs(seq_example,10), 
#'                            Biostrings::pairwiseAlignment, 
#'                            scoreOnly  = TRUE, 
#'                            comp_cores = 1)
#'  
#'    
#' @seealso \code{\link{evalAlignment}}, \code{\link{randomSeqs}}       
#' @import foreach
#' @export  

randSeqDistr <- function(seq, subject, sampled_strings, FUN, ... , comp_cores = 1){
        
        if(!all(is.character(seq),is.character(subject)))
                stop("Please insert a string as seq argument.")
        
        # get sequence length
        l <- nchar(seq)
        s <- nchar(subject)
        i <- NULL
        
        if(length(seq) > 1)
                stop("Please insert a string as seq argument.")
        
        if(length(subject) > 1)
                stop("Please insert a string as subject argument.")
        
        if((l == 1) | (s == 1))
                warning("Are you sure that you want to randomly sample one letter?")
        
        align.fun <- match.fun(FUN)
        n_samples <- length(sampled_strings) 
        res <- vector(mode = "list", length = n_samples)
        
        if(comp_cores == 1)
                res <- sapply(1:n_samples, function(x) align.fun(subject , sampled_strings[x], ...))
        
        
        if(comp_cores > 1){
                
                cores <- parallel::detectCores()
                
                if(comp_cores > cores)
                        stop("The number of cores you specified in the comp_cores argument exeeds the number of cores 
                             available on your machine.")
                
                par_cores <- parallel::makeForkCluster(comp_cores)
                doParallel::registerDoParallel(par_cores)
                
                res <- foreach::foreach(i = 1:n_samples,.combine = "c") %dopar% { (function(x) align.fun(subject , sampled_strings[x], ...))(i) }
                
                parallel::stopCluster(par_cores)
                
        
        }

        return(res)
}
