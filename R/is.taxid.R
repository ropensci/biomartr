is.taxid <- function(x) {
    return(stringr::str_count(x, "[:digit:]") == nchar(x))
}
