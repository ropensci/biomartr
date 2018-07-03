tidy_name <- function(x) {
    new_spec_names <- unlist(lapply(x, function(y) {
        split_str <- unlist(stringr::str_split(y, "_"))
        if (y == split_str) {
            return(y)
        } else {
            return(paste0(
                stringr::str_to_upper(stringr::str_sub(split_str[1], 1, 1)),
                split_str[2],
                collapse = ""
            ))
        }
        
    }))
    
    return(new_spec_names)
}
