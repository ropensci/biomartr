#' @title Main function to query the biomart database
#' @description This function takes a set of gene ids and the biomart specifications
#' and performs a biomart query for the given set of gene ids.
#' @param genes a character vector storing the gene ids of a organisms of interest to be queried against biomart.
#' @param mart a character string specifying the mart to be used, e.g. mart = "ensembl".
#' @param dataset a character string specifying the dataset within the mart to be used, e.g. dataset = "hsapiens_gene_ensembl"
#' @param attributes a character vector specifying the attributes that shall be used, e.g. attributes = c("start_position","end_position","description")
#' @param filters a character vector specifying the filter (query key) for the biomart query, e.g. filter = "ensembl_gene_id"
#' @param ... additional parameters for the biomaRt::getBM() function
#' @author Hajk-Georg Drost
#' @details The biomart() function is the main query function of the biomartr package.
#' It enables to fastly access annotations of a given gene set based on the biomaRt package
#' implemented by Steffen Durinck. 
#' @examples 
#' # the initial biomaRt workflow would work as follows:
#' 
#' # 1) select a mart and data set
#' mart <- useDataset("athaliana_eg_gene", mart = useMart("plants_mart_22"))
#' 
#' # 2) run a biomart query using the getBM() function
#' # and specify the attributes and filter arguments
#' geneSet <- c("AT1G06090", "AT1G06100", "AT1G06110", "AT1G06120", "AT1G06130", "AT1G06200")
#'  
#' resultTable <- getBM(attributes = c("start_position","end_position","description"),
#'                      filters = "tair_locus", values = geneSet, mart = mart)
#'                      
#' # for faster query access and easier query logic the
#' # biomart() function combines this workflow
#'                      
#' # using mart: 'plants_mart_22', dataset: "athaliana_eg_gene"
#' # attributes: c("start_position","end_position","description")
#' # for an example gene set of Arabidopsis thaliana: c("AT1G06090", "AT1G06100", "AT1G06110", "AT1G06120", "AT1G06130", "AT1G06200") 
#' 
#' biomart(genes = c("AT1G06090", "AT1G06100", "AT1G06110", "AT1G06120", "AT1G06130", "AT1G06200"),
#'         mart = "plants_mart_22", dataset = "athaliana_eg_gene",attributes = c("start_position","end_position","description"),
#'         filters = "tair_locus") 
#' 
#' @return A data.frame storing the initial query gene vector in the first column, the output 
#' gene vector in the second column, and all attributes in the following columns.
#' @export
biomart <- function(genes,mart,dataset,attributes,filters,...){
        
        m <- useMart(mart)
        d <- useDataset(dataset = dataset,mart = m)
        
        ### establishing a biomaRt connection and retrieving the information for the given gene list
        query <- getBM(attributes = c(filters,attributes),filters = filters, values = genes, mart = d, uniqueRows = TRUE,...)
        
        # sometimes the query order is not returned correctly by biomart,
        # therefore the query order is being checked 
        inter <- na.omit(match(query[ , 1], genes))
        
        # biomart output as data.frame
        tbl_biomart <- data.frame(genes,genes[inter],query)
        
        colnames(tbl_biomart) <- c("filter_input","filter_output",attributes)
        ### returning a data.frame storing the input gene id's and their corresponding attributes
        return(tbl_biomart)
        
}







