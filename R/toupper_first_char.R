toupper_first_char <- function(string) {
    
    return(stringr::str_c(stringr::str_to_upper(stringr::str_sub(string,1,1)), stringr::str_sub(string,2,nchar(string)), collapse = ""))
}
