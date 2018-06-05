test_url_status <- function(url, organism) {
    
    test_status <- curl::curl_fetch_memory(url)
    
    if (test_status$status_code == 200) {
        json.qry.info <-
            jsonlite::fromJSON(url)
        return(json.qry.info)
    } else {
        warning(
            "The url: ",
            url,
            " could not be reached. It seems like the organism '",
            organism,
            "' does not exist in this database. Could it be that the organism name is misspelled? Thus, download has been omitted.",
            call. = FALSE
        )
        return(FALSE)
    }
}
