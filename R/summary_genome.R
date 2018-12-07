#' @title Retrieve summary statistics for a genome assembly file
#' @description A summary statistics of specific genome features is returned.
#' @param file file path to a genome assembly file in \code{fasta} format.
#' @param organism character string specifying the organism at hand.
#' @author Hajk-Georg Drost
#' @details 
#' The summary statistics include:
#' \itemize{
#' \item \code{genome_size_mbp}: Genome size in mega base pairs
#' \item \code{n50_mbp}: The N50 contig size of the genome assembly in mega base pairs
#' \item \code{n_seqs}: The number of chromosomes/scaffolds/contigs of the genome assembly file
#' \item \code{n_nnn}: The absolute number of NNN's 
#' (over all chromosomes/scaffolds/contigs) in the genome assembly file
#' \item \code{rel_nnn}: The percentage (relative frequency) of NNN's 
#' (over all chromosomes/scaffolds/contigs) compared to the total number of 
#' nucleotides in the genome assembly file
#' \item \code{genome_entropy}: The \code{Shannon Entropy} of the genome assembly file
#' (median entropy over all individual chromosome entropies)}
#' \item \code{n_gc}: The total number of GC's 
#' (over all chromosomes/scaffolds/contigs) in the genome assembly file
#' \item \code{rel_gc}: The (relative frequency) of GC's 
#' (over all chromosomes/scaffolds/contigs) compared to the total number of 
#' nucleotides in the genome assembly file
#' @seealso \code{\link{getCollection}}, \code{\link{getGenome}}, \code{\link{read_genome}}
#' @export 
summary_genome <- function(file,
                           organism) {
        if (!file.exists(file))
                stop("Please provide a valid file path to your genome assembly file.", call. = FALSE)
        
        genome_seq <- Biostrings::readDNAStringSet(filepath = file, format = "fasta")
        # determine letter frequency as probablity distr (relative frequency of nucleotides) 
        nucl_feq <- Biostrings::letterFrequency(genome_seq, letters = c("A","C","G","T"), as.prob = TRUE)
        # compute the median rel feq over all chromosomes
        nucl_re_freq <- apply(nucl_feq, 1, philentropy::H)
        # compute the entropy of the nucleotide distributions
        genome_entropy <- median(nucl_re_freq)
        
        # compute genome size in Mega base pairs
        genome_size_nucl_mbp <-
                sum(as.numeric(genome_seq@ranges@width)) / 1000000
        
        # total number of all nucleotides in the genome
        length_all_nucl <- sum(as.numeric(genome_seq@ranges@width))
        # count absolute number of NNN's in genome
        NNN_abs <- sum(as.numeric(Biostrings::vcountPattern("N", genome_seq)))
        
        # relative frequency of NNN's in genome
        NNN_freq <- NNN_abs / length_all_nucl
        
        # count absolute number of GC's in genome
        GC_abs <- sum(as.numeric(Biostrings::vcountPattern("GC", genome_seq)))
        
        # relative frequency of GC's in genome
        GC_freq <- GC_abs / length_all_nucl
        
        # compute N50 of genome assembly
        genome_N50 <- Biostrings::N50(Biostrings::width(genome_seq)) / 1000000
        
        res <- tibble::tibble(organism = organism,
                              genome_size_mbp = genome_size_nucl_mbp,
                              n50_mbp = genome_N50,
                              n_seqs = length(genome_seq),
                              n_nnn = NNN_abs,
                              rel_nnn = NNN_freq,
                              genome_entropy = genome_entropy,
                              n_gc = GC_abs,
                              rel_gc = GC_freq
                              )
        
        return(res)
}






