tidy_name <- function(x) {
    new_spec_names <- unlist(lapply(x, function(y) {
        split_str <- unlist(stringr::str_split(y, "_"))
            return(paste0(
                stringr::str_to_upper(stringr::str_sub(split_str[1], 1, 1)),
                stringr::str_sub(split_str[1], 2, nchar(split_str[1])),
                stringr::str_to_upper(stringr::str_sub(split_str[2],1,1)),
                stringr::str_sub(split_str[2], 2, nchar(split_str[2])),
                collapse = ""
            ))
    }))
    
    return(new_spec_names)
}
