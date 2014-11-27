#' @title Function to quantify the statistical significance of a given pairwise alignment
#' @description This function allows to quantify the statistical significance of a given pairwise alignment
#' between a query and subject sequence based on a sampled score distribution returned by \code{\link{randSeqDistr}}.
#' @param seq a character vector storing a sequence as string for which random sequences shall be computed.
#' @param subject a character vector storing a subject sequence as string to which \code{seq} shall be pairwise aligned.
#' @param sample_size a numeric value specifying the number of random sequences that shall be returned.
#' @param FUN a pairwise alignment function such as \code{\link[Biostrings]{pairwiseAlignment}} or any other function
#' that takes sequence arguments as first and second input.
#' @param ... additional arguments that shall be passed to \code{FUN}.
#' @param fit_distr a character string specifying the probability distribution that shall be fitted to the histogram
#' of scores returned by \code{\link{randSeqDistr}}. See \code{\link[fitdistrplus]{fitdist}} when method = "mme" for details.
#' A special case is \code{fit_distr} = \code{"simple"}. This way simply the relative frequency of random scores that are greater than
#' the real alignment score is returned as p-value.
#' @param gof a logical value specifying whether or not godness of fit measures shall be printed to the console.
#' @param comp_cores a numeric value specifying the number of cores you want to use for multicore processing.
#' @details
#' The test statistic is developed using moment matching estimation of a given probability distribution that
#' is fitted to the alignment score vector returned by \code{\link{randSeqDistr}}. The corresponding distribution 
#' parameters are estimated by the \code{\link[fitdistrplus]{fitdist}} and the p-value quantifying the statistical 
#' significance of the pairwise alignment of the input sequences is returned.
#' 
#' The following distributions can be fitted to the alignment score distribution:
#' 
#' \itemize{
#'  \item \code{\link{pnorm}}
#'  \item \code{\link{plnorm}}
#'  \item \code{\link{ppois}}
#'  \item \code{\link{pexp}}
#'  \item \code{\link{pgamma}}
#'  \item \code{\link{pnbinom}}
#'  \item \code{\link{pgeom}}
#'  \item \code{\link{pbeta}}
#' }
#' 
#' A special case is \code{fit_distr} = \code{"simple"}. This way simply the relative frequency of random scores that are greater than
#' the real alignment score is returned as p-value.
#' 
#' @author Hajk-Georg Drost
#' @return a p-value quantifying the statistical significance of the pairwise alignment of the input sequences.
#' @examples
#' 
#' seq_example <- "MEDQVGFGF"
#' subject_example <- "AYAIDPTPAF"
#' 
#' p_val_align <- evalAlignment(seq_example, subject_example, 10, 
#'                              Biostrings::pairwiseAlignment, 
#'                              scoreOnly=TRUE, fit_distr = "norm", 
#'                              comp_cores = 1)
#'               
#' @seealso \code{\link{randSeqDistr}}, \code{\link{randomSeqs}}, \code{\link[fitdistrplus]{fitdist}}
#' @export

evalAlignment <- function(seq, subject, sample_size, FUN, ... ,fit_distr = "norm",gof = FALSE, comp_cores = 1){
        
        if(!is.element(fit_distr,c("norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta","simple")))
                stop("Please choose a distribution to fit your alignment scores that is supported by this function.")
        
        align.fun <- match.fun(FUN)
        # random sequence vector
        r_seqs <- vector(mode = "character", length = sample_size)
        # alignment score vector of random seqs
        scores <- vector(mode = "numeric", length = sample_size)
        
        p_val <- vector(mode = "numeric", length = 1)
        real_score <- vector(mode = "numeric", length = 1)
        
        # determine the score of the real pairwise alignment
        real_score <- align.fun(seq, subject, ...)
        
        # determine random scores
        r_seqs <- randomSeqs(seq = seq,sample_size = sample_size)
        scores <- randSeqDistr(seq = seq, subject = subject, 
                               sampled_strings = r_seqs, 
                               FUN = align.fun, ..., 
                               comp_cores = comp_cores)
        
        
        
        if(fit_distr == "norm"){
                # paramter estimates of the normal distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- pnorm(real_score, mean = est$estimate[1], sd = est$estimate[2], lower.tail = FALSE)
        }
        
        if(fit_distr == "gamma"){
                if(range(scores)[1] < 0)
                        stop("Please choose a distribution to fit your alignment scores that supports negative values.")
                
                # paramter estimates of the gamma distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- pgamma(real_score, shape = est$estimate[1], rate = est$estimate[2], lower.tail = FALSE)
        }
        
        
        if(fit_distr == "lnorm"){
                # paramter estimates of the log-normal distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- plnorm(real_score, meanlog = est$estimate[1], sdlog = est$estimate[2], lower.tail = FALSE)
        }
        
        if(fit_distr == "pois"){
                
                if(range(scores)[1] < 0)
                        stop("Please choose a distribution to fit your alignment scores that supports negative values.")
                
                # paramter estimates of the poisson distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- ppois(real_score, lambda = est$estimate[1], lower.tail = FALSE)
        }
        
        if(fit_distr == "exp"){
                
                if(range(scores)[1] < 0)
                        stop("Please choose a distribution to fit your alignment scores that supports negative values.")
                
                # paramter estimates of the exponential distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- pexp(real_score, rate = est$estimate[1], lower.tail = FALSE)
        }
        
        if(fit_distr == "nbinom"){
                
                if(range(scores)[1] < 0)
                        stop("Please choose a distribution to fit your alignment scores that supports negative values.")
                
                # paramter estimates of the negative binomial distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- pnbinom(real_score, size = est$estimate[1], mu = est$estimate[2], lower.tail = FALSE)
        }
        
        if(fit_distr == "geom"){
                
                if(range(scores)[1] < 0)
                        stop("Please choose a distribution to fit your alignment scores that supports negative values.")
                
                if(!is.integer(real_score))
                        stop("Please specify a discrete (integer) input vector.")
                
                # paramter estimates of the geometric distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- pgeom(real_score, prob = est$estimate[1], lower.tail = FALSE)
        }
        
        if(fit_distr == "beta"){
                
                if(range(scores)[1] < 0)
                        stop("Please choose a distribution to fit your alignment scores that supports negative values.")
                
                # paramter estimates of the beta distribution
                # estimated by moment matching
                est <- fitdistrplus::fitdist(scores, distr = fit_distr, method = "mme")
                
                if(gof){
                        print(fitdistrplus::gofstat(est))
                        cat("\n")
                }
                
                p_val <- pbeta(real_score, shape1 = est$estimate[1], shape2 = est$estimate[2], lower.tail = FALSE)
        }
        
        if(fit_distr == "simple"){
                
                p_val <- sum(scores > real_score) / length(scores)
        }
        
        names(p_val) <- "p.value"
        
        return(p_val)
        
}





