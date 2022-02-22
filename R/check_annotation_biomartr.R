#' @title Check whether an annotation file contains outlier lines
#' @description Some annotation files include lines with character lengths greater than 65000. 
#' This causes problems when trying to import such annotation files into R using \code{import}.
#' To overcome this issue, this function screens for such lines
#' in a given annotation file and removes these lines so that
#' \code{import} can handle the file.
#' @param annotation_file a file path to the annotation file.
#' @param remove_annotation_outliers shall outlier lines be removed from the input \code{annotation_file}? 
#' If yes, then the initial \code{annotation_file} will be overwritten and the removed outlier lines will be stored at \code{\link{tempdir}}
#' for further exploration.
#' @author Hajk-Georg Drost
#' @examples \dontrun{
#' # download an example annotation file from NCBI RefSeq
#' Ath_path <- biomartr::getGFF(organism = "Arabidopsis thaliana")
#' # run annotation file check on the downloaded file
#' biomartr::check_annotation_biomartr(Ath_path)
#' # several outlier lines were detected, thus we re-run the
#' # function using 'remove_annotation_outliers = TRUE'
#' # to remove the outliers and overwrite the file
#' biomartr::check_annotation_biomartr(Ath_path, remove_annotation_outliers = TRUE)
#' }
#' @export

check_annotation_biomartr <- function(annotation_file, remove_annotation_outliers = FALSE) {
        
        if (!file.exists(annotation_file))
                stop("The file '", annotation_file, "' does not seem to exist. Please provide a valid path to the annotation file.", call. = FALSE)
        
        message("Importing '", annotation_file, "' ...")
        import_annotation <- readr::read_lines(annotation_file)
        outlier_lines <- which(nchar(import_annotation) > 65000)
        
        if (length(outlier_lines) == 0)
                return(annotation_file)
        
        if (length(outlier_lines) > 0 & !remove_annotation_outliers) {
                warning("Your annotation file '", annotation_file, "' contains ", length(outlier_lines), " lines that have more than 65000 characters.",
                        "This will cause the annotation import function to fail importing the annotation file into R.",
                        " If you wish to remove the outlier lines and overwrite the existing annotation file then re-run this function and specify 'remove_annotation_outliers = TRUE'. A file with the removed lines will then be stored in the temp directory for you to explore.")
                return(annotation_file)
        }
        
        if (length(outlier_lines) > 0 & remove_annotation_outliers) {
                message("Reading annotation file '", annotation_file, "' and removing all outlier lines with number of characters greater 65000 ...")
                message("Overwriting '", annotation_file, "' with removed outlier lines ...")
                
                file_extension_check <- unlist(stringr::str_split(annotation_file, "[.]"))
                if (file_extension_check[length(file_extension_check)] == "gz"){
                        message("Unzipping file ", annotation_file,"' ...")
                        R.utils::gunzip(annotation_file, destname = paste0(file_extension_check[1],".", file_extension_check[2]))
                }
                
                readr::write_lines(import_annotation[-outlier_lines], paste0(file_extension_check[1],".", file_extension_check[2]))
                
                message("The new annotation file was created and has been stored at '", paste0(file_extension_check[1],".", file_extension_check[2]),"'.")
                
                outlier_output_file <- file.path(tempdir(), paste0(basename(annotation_file),"_outlier_lines.txt"))
                
                readr::write_lines(import_annotation[outlier_lines], outlier_output_file)
                message("The outlier lines were stored in a separate file to explore at '", outlier_output_file, "'.")
                return(paste0(file_extension_check[1],".", file_extension_check[2]))
        }
        
}



