#' @title Gene Ontology Query
#' @description This function takes a gene id as character vector from a given 
#' query organism and returns the corresponding GO terms and additional GO 
#' information.
#' @param organism a character string specifying the scientific name 
#' of a query organism.
#' @param genes a character vector storing the gene ids of a organisms 
#' of interest to be queried against Ensembl Biomart.
#' @param filters a character vector specifying the filter (query key) for 
#' the Ensembl Biomart query, e.g. \code{filter} = \code{"ensembl_gene_id"}.
#' @param ... additional parameters that can be passed to the 
#' \code{\link{biomart}} function.
#' @author Hajk-Georg Drost
#' @details 
#' This function takes the scientific name of a query organism, a set of genes 
#' for which GO terms and additional information shall be retrieved, and a 
#' filter argument that specifies the attribute for the query genes.  
#' @examples
#' \dontrun{ 
#' GO_tbl <- getGO(organism = "Arabidopsis thaliana", 
#'                 genes    = c("AT1G06090", "AT1G06100"),
#'                 filters  = "ensembl_gene_id")
#' 
#' # look at the result
#' head(GO_tbl)
#' }
#' @seealso \code{\link{biomart}}, \code{\link{organismFilters}}, 
#' \code{\link{organismBM}}, \code{\link[biomaRt]{getBM}}, 
#' \code{\link{getMarts}}, \code{\link{getDatasets}}, 
#' \code{\link{getFilters}}
#' @export

getGO <-
    function(organism,
             genes,
             filters,
             ...) {
        
    name  <- NULL

    orgAttr <-
        organismAttributes(organism = organism, topic = "go")
    
    GOattributes <- c("go_linkage_type",
                      "goslim_goa_accession",
                      "goslim_goa_description")
    
    GOattr_df <-
        dplyr::filter(orgAttr, name %in% GOattributes)
    
    if (dim(GOattr_df)[1] == 0)
        stop("Unfortunately for '",
             organism,
             "' no GO attributes could be found.")
    
    m <- names(table(GOattr_df[ , "mart"]))
    d <- names(table(GOattr_df[ , "dataset"]))
    
    if (m %in% c("ensembl")) {
        GOattributes <- "go_linkage_type"
    } else {
        GOattributes <- c("goslim_goa_description", "goslim_goa_accession")
    }
    
    if ((length(m) > 1) | (length(d) > 1))
        stop(
            "GO related attributes have been found in more than one mart or 
            dataset: mart = ",
            m,
            " ; dataset = ",
            d
        )
    
    return(
        biomart(
            genes      = genes,
            mart       = m,
            dataset    = d,
            attributes = GOattributes,
            filters    = filters,
            ...
        )
    )
}


