existingOrganisms <- function(path, .type) {
    .files <- list.files(path)
    .files <- .files[!stringr::str_detect(.files, "doc_")]
    .files <- .files[!stringr::str_detect(.files, "md5cheksum")]
    
    .orgs <- unlist(purrr::map(.files, function(x, type = .type) {
        .data <- unlist(stringr::str_split(x, "_"))
        if (length(.data) > 0){
            
            which_found <- which(.data == type)
            if (length(which_found) > 0) {
                .data <- .data[-c(which_found:length(.data))]
                return(paste0(.data, collapse = " "))
            } else {
             warning("It seems like there are some files in download folder that are neither pre-downloaded species files nor doc_ or md5checksum files.")   
            }
        }
    }))
    
    return (.orgs)
}
