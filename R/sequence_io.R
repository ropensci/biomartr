#' @title Read the genome of a given Organism
#' @description This function reads an organism specific genome stored in a defined file format.
#' @param file a character string specifying the path to the file storing the genome.
#' @param format a character string specifying the file format used to store the genome, e.g. "fasta", "gbk".
#' @param ... additional arguments that are used by the seqinr::read.fasta() function.
#' @author Hajk-Georg Drost
#' @details The \code{read.genome} function takes a string specifying the path to the genome file
#' of interest as first argument.
#'
#' It is possible to read in different genome file standards such as \emph{fasta} or \emph{genebank}.
#' Genomes stored in fasta files can be downloaded from http://ensemblgenomes.org/info/genomes.
#'
#' @examples \dontrun{
#' # reading a genome stored in a fasta file
#' Ath.genome <- read.genome(system.file('seqs/ortho_thal_cds.fasta', package = 'orthologr'),
#'                            format = "fasta")
#' }
#'
#' @return A data.table storing the gene id in the first column and the corresponding
#' sequence as string in the second column.
#' @export
read_genome <- function(file, format, ...){
    
     if (!is.element(format, c("fasta", "gbk")))
        stop("Please choose a file format that is supported by this function.")
    
    geneids <- NULL
    
    tryCatch({
        genome <-
            Biostrings::readDNAStringSet(filepath = file, format = format, ...)
        genome_names <-
            as.vector(unlist(sapply(genome@ranges@NAMES, function(x) {
                return(strsplit(x, " ")[[1]][1])
            })))
        genome.dt <-
            data.table::data.table(geneids = genome_names,
                                   seqs = tolower(as.character(genome)))
        
        data.table::setkey(genome.dt, geneids)
        
    }, error = function(e) {
        stop(
            paste0(
                "File ",
                file,
                " could not be read properly. \n",
                "Please make sure that ",
                file,
                " contains only DNA sequences and is in ",
                format,
                " format."
            )
        )
    })
    
    return(genome.dt)
}


#' @title Read the proteome of a given Organism
#' @description This function reads an organism specific proteome stored in a defined file format.
#' @param file a character string specifying the path to the file storing the proteome.
#' @param format a character string specifying the file format used to store the proteome, e.g. "fasta", "gbk".
#' @param ... additional arguments that are used by the seqinr::read.fasta() function.
#' @author Hajk-Georg Drost
#' @details The \code{read.proteome} function takes a string specifying the path to the proteome file
#' of interest as first argument.
#'
#' It is possible to read in different proteome file standards such as \emph{fasta} or \emph{genebank}.
#'
#' Proteomes stored in fasta files can be downloaded from http://www.ebi.ac.uk/reference_proteomes.
#'
#' @examples \dontrun{
#' # reading a proteome stored in a fasta file
#' Ath.proteome <- read.proteome(system.file('seqs/ortho_thal_aa.fasta', package = 'orthologr'),
#'                                format = "fasta")
#' }
#'
#' @return A data.table storing the gene id in the first column and the corresponding
#' sequence as string in the second column.
#' @import data.table
#' @export
read_proteome <- function(file, format, ...){
        
    if (!is.element(format, c("fasta", "gbk")))
        stop("Please choose a file format that is supported by this function.")
    
    geneids <- NULL
    
    tryCatch({
        proteome <-
            Biostrings::readAAStringSet(filepath = file, format = format, ...)
        proteome_names <-
            as.vector(unlist(sapply(proteome@ranges@NAMES, function(x) {
                return(strsplit(x, " ")[[1]][1])
            })))
        proteome.dt <-
            data.table::data.table(geneids = proteome_names,
                                   seqs = tolower(as.character(proteome)))
        
        data.table::setkey(proteome.dt, geneids)
        
    }, error = function(e) {
        stop(
            paste0(
                "File ",
                file,
                " could not be read properly. \n",
                "Please make sure that ",
                file,
                " contains only amino acid sequences and is in ",
                format,
                " format."
            )
        )
    })

    return(proteome.dt)
}


#' @title Read the CDS of a given Organism
#' @description This function reads an organism specific CDS stored in a defined file format.
#' @param file a character string specifying the path to the file storing the CDS.
#' @param format a character string specifying the file format used to store the CDS, e.g. "fasta", "gbk".
#' @param delete_corrupt a logical value specifying whether potential CDS sequences that cannot be divided by 3 shall be
#' be excluded from the the dataset. Default is \code{delete_corrupt = FALSE}.
#' @param ... additional arguments that are used by the seqinr::read.fasta() function.
#' @author Hajk-Georg Drost
#' @details The \code{read.cds} function takes a string specifying the path to the cds file
#' of interest as first argument.
#'
#' It is possible to read in different proteome file standards such as \emph{fasta} or \emph{genebank}.
#'
#' CDS stored in fasta files can be downloaded from http://www.ensembl.org/info/data/ftp/index.html.
#'
#' @examples \dontrun{
#' # reading a cds file stored in fasta format
#' Ath.cds <- read.cds(system.file('seqs/ortho_thal_cds.fasta', package = 'orthologr'),
#'                     format = "fasta")
#' }
#'
#' @return A data.table storing the gene id in the first column and the corresponding
#' sequence as string in the second column.
#' @import data.table
#' @export
read_cds <- function(file, format, delete_corrupt = FALSE, ...){
        
    if(!is.element(format, c("fasta", "gbk")))
        stop("Please choose a file format that is supported by this function.")
    
    geneids <- seqs <- NULL
    
    tryCatch({
        cds_file <-
            Biostrings::readBStringSet(filepath = file, format = format, ...)
        
        cds_names <-
            as.vector(unlist(sapply(cds_file@ranges@NAMES, function(x) {
                return(strsplit(x, " ")[[1]][1])
            })))
        
        cds.dt <-
            data.table::data.table(geneids = cds_names ,
                                   seqs = tolower(as.character(cds_file)))
        
        
        data.table::setkey(cds.dt, geneids)
        
        mod3 <-
            function(x) {
                return((nchar(x) %% 3) == 0)
            }
        
        all_triplets <- cds.dt[, mod3(seqs)]
        
        if (!all(all_triplets)) {
            warning(
                paste0(
                    "There have been ",
                    length(which(!all_triplets)),
                    " genes found that cannot be divided by 3. To delete these sequences please specify the 'delete_corrupt = TRUE' argument."
                )
            )
        }
    }, error = function(e) {
        stop(
            paste0(
                "File ",
                file,
                " could not be read properly. \n",
                "Please make sure that ",
                file,
                " contains only CDS sequences and is in ",
                format,
                " format."
            )
        )
    })
    
    if (!all(all_triplets)) {
        if (delete_corrupt)
            return(cds.dt[-which(!all_triplets) , list(geneids, seqs)])
        
        if (!delete_corrupt)
            return(cds.dt)
        
    } else {
        return(cds.dt)
    }
        
}

