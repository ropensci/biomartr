#' @title Transform 1st character of a string to upper case
#' @description Helper function to only transform 1st character of a string to upper case.
#' @param string character string that shall be transformed.
#' @author Hajk-Georg Drost
#' @noRd
toupper_first_char <- function(string) {
    return(stringr::str_c(stringr::str_to_upper(stringr::str_sub(string,1,1)),
                          stringr::str_sub(string,2,nchar(string)),
                          collapse = ""))
}
