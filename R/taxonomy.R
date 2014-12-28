#' @title Retrieving Taxonomic Information of a Query Organism
#' @description This function takes the scientific name of a query organism
#' and returns selected output formats of taxonomic information for the corresponding organism.
#' @param organism a character string specifying the scientific name of a query organism.
#' @param db a character string specifying the database to query, e.g. \code{db} = \code{"itis"}, \code{"col"}, or \code{"ncbi"}.
#' @param output a character string specifying the taxonomic information that shall be returned. 
#' Implemented are: \code{output} = \code{"classification"}, \code{"taxid"}, \code{"children"}.
#' @details This function is based on the powerful package \pkg{taxize} and implements
#' the customized retrieval of taxonomic information for a query organism. 
#' @author Hajk-Georg Drost
#' @examples
#' 
#' # retrieving the taxonomic hierarchy of "Arabidopsis thaliana"
#' # from NCBI Taxonomy
#' taxonomy("Arabidopsis thaliana",db = "ncbi")
#' 
#' # the same can be applied to databases : "ncbi", "eol", and "itis"
#' # taxonomy("Arabidopsis thaliana",db = "ncbi")
#' # taxonomy("Arabidopsis thaliana",db = "col")
#' # taxonomy("Arabidopsis thaliana",db = "itis")
#' 
#' # retrieving the taxonomic hierarchy of "Arabidopsis"
#' taxonomy("Arabidopsis",db = "ncbi") # analogous : db = "ncbi", "eol", or "itis"
#' 
#' # or just "Arabidopsis"
#' # taxonomy("Arabidopsis",db = "ncbi")
#' 
#' # retrieving the taxonomy id of the query organism and in the correspondning database
#' taxonomy("Arabidopsis thaliana",db = "ncbi", output = "taxid")
#' 
#' # the same can be applied to databases : "ncbi", "eol", and "itis"
#' # taxonomy("Arabidopsis thaliana",db = "ncbi", output = "taxid")
#' # taxonomy("Arabidopsis thaliana",db = "col", output = "taxid")
#' # taxonomy("Arabidopsis thaliana",db = "itis", output = "taxid")
#' 
#' 
#' # retrieve children taxa of the query organism stored in the correspondning database
#' taxonomy("Arabidopsis",db = "ncbi", output = "children")
#' 
#' #' # the same can be applied to databases : "ncbi", "eol", and "itis"
#' # taxonomy("Arabidopsis thaliana",db = "ncbi", output = "children")
#' # taxonomy("Arabidopsis thaliana",db = "col", output = "children")
#' # taxonomy("Arabidopsis thaliana",db = "itis", output = "children")
#' 
#' @references
#' 
#' Scott Chamberlain and Eduard Szocs (2013). taxize - taxonomic search and retrieval in R. F1000Research,
#' 2:191. URL: http://f1000research.com/articles/2-191/v2.
#' 
#' Scott Chamberlain, Eduard Szocs, Carl Boettiger, Karthik Ram, Ignasi Bartomeus, and John Baumgartner
#' (2014) taxize: Taxonomic information from around the web. R package version 0.3.0.
#' https://github.com/ropensci/taxize
#' @export

taxonomy <- function(organism, db = "ncbi", output = "classification"){
        
        if(!is.element(output,c("classification","taxid","children")))
                stop("The output '",output,"' is not supported by this function.")
        
        if(output == "classification"){
                
                species <- strsplit(organism," ")[[1]][1]
        
                # retrieve the taxonomic hierarchy for the query species
                tax_hierarchy <- as.data.frame(taxize::classification(species, db = db)[[1]])
        
                # retrieve the members (children/organisms) of the correspondning species
                tax_child_organisms <- as.data.frame(taxize::children(species, db = db)[[1]])
                colnames(tax_child_organisms) <- c("id","name","rank")
                tax_child_organisms <- tax_child_organisms[ , c("name","rank","id")]
                
                # filter for the query organism
                tax_query_org <- dplyr::filter(tax_child_organisms, name == organism)
                
                return(rbind(tax_hierarchy,tax_query_org))
        }
        
        if(output == "taxid"){
                
                split_org <- strsplit(organism," ")[[1]]
                
                if(length(split_org) == 1){
                        
                        # retrieve the taxonomic hierarchy for the query species
                        tax_hierarchy <- as.data.frame(taxize::classification(split_org, db = db)[[1]])
                        
                        return(dplyr::select(dplyr::filter(tax_hierarchy, name == split_org),id))
                }
                
                if(length(split_org) > 1){
                 
                        # retrieve the members (children/organisms) of the correspondning species
                        tax_child_organisms <- as.data.frame(taxize::children(split_org[1], db = db)[[1]])
                        colnames(tax_child_organisms) <- c("id","name","rank")
                        tax_child_organisms <- tax_child_organisms[ , c("name","rank","id")]
                        
                        # filter for the query organism
                        tax_query_org <- dplyr::filter(tax_child_organisms, name == organism)
                        
                        return(dplyr::select(tax_query_org,id))
                }
        }
        
        
        if(output == "children"){
                
                split_org <- strsplit(organism," ")[[1]]
                
                if(length(split_org) == 1){
                        
                        return(as.data.frame(taxize::children(split_org, db = db)[[1]]))
                        
                } else {
                        
                        stop("Please enter a hierarchical order above 'species', e.g. 'Arabidopsis' instead of 'Arabidopsis thaliana'.")
                }
                
                
        }
}



