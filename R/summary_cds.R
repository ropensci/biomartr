#' @title Retrieve summary statistics for a coding sequence (CDS) file
#' @description A summary statistics of specific CDS features is returned.
#' @param file file path to a CDS file in \code{fasta} format.
#' @param organism character string specifying the organism at hand.
#' @author Hajk-Georg Drost
#' @details 
#' The summary statistics include:
#' \itemize{
#' \item \code{total_seqs}: 
#' \item \code{nnn_abs}: The total number of NNN's 
#' (over all chromosomes/scaffolds/contigs) in all coding sequences combined
#' \item \code{nnn_perc}: The percentage (relative frequency) of NNN's 
#' (over all chromosomes/scaffolds/contigs) compared to the total number of 
#' nucleotides of all coding sequences
#' }
#' @seealso \code{\link{getCollection}}, \code{\link{getCDS}}, \code{\link{read_cds}}, \code{\link{summary_genome}}
#' @export 
summary_cds <- function(file,
                        organism) {
        if (!file.exists(file))
                stop("Please provide a valid file path to your CDS file.", call. = FALSE)
        
        cds_seq <- Biostrings::readDNAStringSet(filepath = file, format = "fasta")
       if (length(cds_seq) == 0)
               stop("Please provide a file that contains CDS sequences.", call. = FALSE)
        
        total_seqs <- length(cds_seq)
        total_corrupt_cds <- sum(cds_seq@ranges@width %% 3 > 0)
        corrupt_cds_perc <- total_corrupt_cds / total_seqs
        
        all_cds_seqs <- as.character(cds_seq)
        
        start_AND_end <- sum(sapply(all_cds_seqs, function(x) {
                triplet_start <- stringr::str_sub(x, 1, 3)
                triplet_end <-
                        stringr::str_sub(x, nchar(x) - 2, nchar(x))
                
                return((triplet_start == "ATG") &&
                               (triplet_end == "TGA"))
        }))
        
        ONLY_start <- sum(sapply(all_cds_seqs, function(x) {
                triplet_start <- stringr::str_sub(x, 1, 3)
                return(triplet_start == "ATG")
        }))
        
        start_with_N <- sum(sapply(all_cds_seqs, function(x) {
                triplet_start <- stringr::str_sub(x, 1, 1)
                return(triplet_start == "N")
        }))
        
        ONLY_end <- sum(sapply(all_cds_seqs, function(x) {
                triplet_end <-
                        stringr::str_sub(x, nchar(x) - 2, nchar(x))
                
                return(triplet_end == "TGA")
        }))
        
        end_with_N <- sum(sapply(all_cds_seqs, function(x) {
                triplet_end <-
                        stringr::str_sub(x, nchar(x), nchar(x))
                
                return(triplet_end == "N")
        }))
        
        # count absolute number of NNN's in genome
        NNN_abs <- sum(as.numeric(Biostrings::vcountPattern("N", cds_seq)))
        
        # relative frequency of NNN's in genome
        NNN_freq <-
                sum(as.numeric(Biostrings::vcountPattern("N", cds_seq))) / sum(as.numeric(cds_seq@ranges@width))
        
        res <- tibble::tibble(organism = organism,
                              n_seqs = total_seqs,
                              n_corrupt_cds = total_corrupt_cds,
                              rel_corrupt_cds = corrupt_cds_perc,
                              n_start_atg_and_end_tga = start_AND_end,
                              rel_start_atg_and_end_tga = start_AND_end / total_seqs,
                              n_start_atg = ONLY_start,
                              rel_start_atg = ONLY_start / total_seqs,
                              n_start_n = start_with_N,
                              n_end_tga = ONLY_end,
                              rel_end_tga = ONLY_end / total_seqs,
                              n_end_n = end_with_N,
                              n_nnn = NNN_abs,
                              rel_nnn = NNN_freq
                              
        )
        
        return(res)
}






