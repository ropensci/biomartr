clean_species_names_helper <- function(folder_files, gunzip = TRUE, as.list = TRUE) {

    file_ext <- "[.]*a$"

    if (any(stringr::str_detect(folder_files, "[.]faa.gz$"))) {
        seq_type <- "ncbi_protein"
        file_ext <- "[.]faa$"
    }

    if (any(stringr::str_detect(folder_files, "[.]fna.gz$"))) {
        seq_type <- "ncbi_nucleotide"
        file_ext <- "[.]fna$"
    }
    if (any(stringr::str_detect(folder_files, "[.]gff.gz$"))) {
        seq_type <- "ncbi_gff"
        file_ext <- "[.]gff$"
    }
    if (any(stringr::str_detect(folder_files, "[.]out.gz$"))) {
        seq_type <- "ncbi_rm"
        file_ext <- "[.]out$"
    }
    if (any(stringr::str_detect(folder_files, "[.]gff3.gz$"))) {
        seq_type <- "ensembl_gff3"
        file_ext <- "[.]gff3$"
    }
    if (any(stringr::str_detect(folder_files, "[.]gtf.gz$"))) {
        seq_type <- "ensembl_gtf"
        file_ext <- "[.]gtf$"
    }
    if (any(stringr::str_detect(folder_files, "[.]fa.gz$"))) {
        seq_type <- "ensembl_fasta"
        file_ext <- "[.]fa$"
    }


    # remove doc, md5checksum files, and already unzipped files
    find_doc <- which(stringr::str_detect(folder_files, "doc_"))
    find_md5 <- which(stringr::str_detect(folder_files, "md5checksum"))
    find_documentaion <- which(stringr::str_detect(folder_files, "documentation"))
    find_unzipped_files <- which(stringr::str_detect(folder_files, file_ext))
    find_rs_graphics <- which(stringr::str_detect(folder_files, "rs-graphics-"))

    if (length(c(find_doc, find_md5, find_documentaion, find_unzipped_files)) > 0) {
        folder_files_reduced <- folder_files[-c(find_doc, find_md5, find_documentaion, find_unzipped_files, find_rs_graphics)]
    }

    if (length(folder_files_reduced) == 0) {
        return(folder_files[-c(find_doc, find_md5, find_documentaion)])
    } else {
        input_files <- folder_files_reduced
    }

    input_files_without_appendix <- unlist(lapply(input_files, function(x) return(unlist(stringr::str_split(x, "[.]"))[1])))

    file_ext <- stringr::str_replace(file_ext, "\\$", "")
    file_ext <- stringr::str_replace(file_ext, "\\[.]", "")

    if (gunzip)
        output_files <- paste0(tidy_name(input_files_without_appendix), ".", file_ext)

    if (!gunzip)
        output_files <- paste0(tidy_name(input_files_without_appendix),".",file_ext,".gz")

    if (as.list) return(list(output_files = output_files, input_files = input_files,
                             input_files_without_appendix = input_files_without_appendix))
    return(output_files)
}
