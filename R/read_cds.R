#' @title Import CDS as Biostrings or data.table object
#' @description This function reads an organism specific CDS stored in a
#' defined file format.
#' @param file a character string specifying the path to the file storing
#' the CDS.
#' @param format a character string specifying the file format used to store
#' the genome, e.g. \code{format = "fasta"} (default) or \code{format = "gbk"}.
#' @param obj.type a character string specifying the object stype in which the
#' genomic sequence shall be represented.
#' Either as \code{obj.type = "Biostrings"} (default) or as
#' \code{obj.type = "data.table"}.
#' @param delete_corrupt a logical value specifying whether potential CDS
#' sequences that cannot be divided by 3 shall be
#' be excluded from the the dataset. Default is \code{delete_corrupt = FALSE}.
#' @param ... additional arguments that are used by
#' \code{\link[seqinr]{read.fasta}}.
#' @author Hajk-Georg Drost
#' @details The \code{read.cds} function takes a string specifying the path
#' to the cds file of interest as first argument.
#'
#' It is possible to read in different proteome file standards such as
#' \emph{fasta} or \emph{genebank}.
#'
#' CDS stored in fasta files can be downloaded from
#' http://www.ensembl.org/info/data/ftp/index.html.
#' @return A data.table storing the gene id in the first column and the
#' corresponding sequence as string in the second column.
#' @family cds
#' @family readers
#' @import Biostrings
#' @export
read_cds <-
    function(file,
             format = "fasta",
             obj.type = "Biostrings",
             delete_corrupt = FALSE,
             ...) {
        if (!is.element(format, c("fasta", "gbk")))
            stop("Please choose a file format that is
                 supported by this function.",
                 call. = FALSE)

        if (!is.element(obj.type, c("Biostrings", "data.table")))
            stop(
                "Please specify a valid object type: obj.type = 'Biostrings'
                (default) or obj.type = 'data.table'.",
                call. = FALSE
            )

        if (!file.exists(file))
            stop("The file path you specified does not seem to exist: '", file,"'.", call. = FALSE)

        geneids <- seqs <- NULL

        if (obj.type == "Biostrings") {
            tryCatch({
                cds_file <-
                    Biostrings::readBStringSet(filepath = file,
                                               format = format, ...)
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
                    ),
                    call. = FALSE
                )
            })

            return(cds_file)
        }

        if (obj.type == "data.table") {
            geneids <- seqs <- NULL

            tryCatch({
                cds_file <-
                    Biostrings::readDNAStringSet(filepath = file, format = format, ...)

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

                all_triplets <- as.logical(cds.dt[ , mod3(seqs)])

                n_seqs <- nrow(cds.dt)

            }, error = function(e) {
                stop(
                    "File ",
                    file,
                    " could not be read properly.",
                    "\n",
                    "Please make sure that ",
                    file,
                    " contains only CDS sequences and is in ",
                    format,
                    " format."
                )
            })

            if (!all(all_triplets)) {
                message(
                    "There seem to be ",
                    length(which(!all_triplets)),
                    " coding sequences in your input dataset which cannot be properly divided in base triplets, because their sequence length cannot be divided by 3."
                )
                corrupted_file <-
                    paste0(basename(file), "_corrupted_cds_seqs.fasta")

                message(
                    "A fasta file storing all corrupted coding sequences for inspection was generated and stored at '",
                    file.path(getwd(), corrupted_file),
                    "'."
                )
                message("\n")
                corrupted_seqs <- as.data.frame(cds.dt[which(!all_triplets)])
                seq_vector <- corrupted_seqs$seqs
                names(seq_vector) <- corrupted_seqs$geneids
                corrupted_seqs_biostrings <- Biostrings::DNAStringSet(seq_vector, use.names = TRUE)
                Biostrings::writeXStringSet(corrupted_seqs_biostrings, filepath = corrupted_file)

                if (delete_corrupt) {
                    message(
                        "You chose option 'delete_corrupt = TRUE', thus corrupted coding sequences were removed.",
                        "If after consulting the file '",
                        corrupted_file,
                        "' you still wish to retain all coding sequences please specify the argument 'delete_corrupt = FALSE'."
                    )
                    message("\n")
                    return(cds.dt[-which(!all_triplets) , list(geneids, seqs)])
                }

                if (!delete_corrupt) {
                    message(
                        "You chose option 'delete_corrupt = FALSE', thus corrupted coding sequences were retained for subsequent analyses.")
                    message(
                        "The following modifications were made to the CDS sequences that were not divisible by 3:")
                    message(
                        "- If the sequence had 1 residue nucleotide then the last nucleotide of the sequence was removed.")
                    message(
                        "- If the sequence had 2 residue nucleotides then the last two nucleotides of the sequence were removed.")
                    message(
                        "If after consulting the file '",
                        corrupted_file,
                        "' you wish to remove all corrupted coding sequences please specify the argument 'delete_corrupt = TRUE'."
                    )

                    mod3_residue_1 <-
                        function(x) {
                            return((nchar(x) %% 3) == 1)
                        }

                    mod3_residue_2 <-
                        function(x) {
                            return((nchar(x) %% 3) == 2)
                        }

                    residue_1 <- cds.dt[ , mod3_residue_1(seqs)]
                    residue_2 <- cds.dt[ , mod3_residue_2(seqs)]

                    residue_1_seqs <- as.character(cds.dt[which(residue_1) , seqs])
                    residue_2_seqs <- as.character(cds.dt[which(residue_2) , seqs])

                    residue_1_seqs_vec <- as.character(sapply(residue_1_seqs, function(x) {
                        stringr::str_sub(x, 1, nchar(x) - 1)
                    }))

                    residue_2_seqs_vec <- as.character(sapply(residue_2_seqs, function(x) {
                        stringr::str_sub(x, 1, nchar(x) - 2)
                    }))

                    cds.dt[which(residue_1) , seqs := residue_1_seqs_vec]
                    cds.dt[which(residue_2) , seqs := residue_2_seqs_vec]

                    all_triplets_new <- cds.dt[ , mod3(seqs)]

                    if (any(!all_triplets_new)) {
                        stop("Something went wring during the trimming process. Not all sequences were trimmed properly.", call. = FALSE)
                    } else {
                        message("All corrupted CDS were trimmed.")
                    }

                    n_seqs_new <- nrow(cds.dt)

                    if(!(n_seqs == n_seqs_new))

                         stop("After trimming corrupted CDS some sequences seem to be lost. Please check what might have gone wrong with the sequence trimming.", call. = FALSE)
                    return(cds.dt)
                }
            } else {
                return(cds.dt)
            }
        }
    }
