#' @title List number of available genomes in each taxonomic group
#' @description Users can retrieve the available number of sequenced 
#' genomes per group. Only available for \code{db = "refseq"} and 
#' \code{db = "genbank"}.
#' @param db a character string specifying the database for which genome 
#' availability shall be checked. Available options are:
#' \itemize{
#' \item \code{db = "refseq"} 
#' \item \code{db = "genbank"}
#' }
#' @param kingdom a kingdom specification retrieved by 
#' \code{\link{getKingdoms}}.
#' @param details shall all species corresponding to the specified 
#' \code{kingdom} be returned? Default is \code{details = FALSE}.
#' @author Hajk-Georg Drost
#' @examples
#' \dontrun{
#' # example for refseq
#' listGroups(db = "refseq")
#' # example for genbank
#' listGroups(db = "genbank")
#' ### in case groups should be specified by kingdom
#' # first, retrieve available kingdom names
#' listKingdoms()
#' # now we choose kingdom "bacteria"
#' listGroups(db = "refseq", kingdom = "bacteria")
#' # or
#' listGroups(db = "genbank", kingdom = "bacteria")
#' }
#' @seealso \code{\link{listGenomes}}, \code{\link{is.genome.available}}, 
#' \code{\link{listKingdoms}}
#' @export

listGroups <-
    function(db = "refseq",
             kingdom = "all",
             details = FALSE) {
        if (!is.element(db, c("refseq", "genbank")))
            stop("Unfortunately, only db = 'refseq' and db = 'genbank' 
                 provide group information.")
        
        if (!is.element(kingdom, c(getKingdoms(), "all")))
            stop(
                "Please choose a kingdom that is supported by NCBI RefSeq or 
                NCBI Genbank. See getKingdoms() for details.",
                call. = FALSE
            )
        
        organism_name <- group <- kingdoms <- subgroup <- NULL
        
        listgenomes.data <-
            listGenomes(db = db,
                        type = "group",
                        details = TRUE)
        
        if (kingdom != "all") {
            # case Animals
            if (is.element(kingdom,
                           c(
                               "invertebrate",
                               "vertebrate_mammalian",
                               "vertebrate_other"
                           ))) {
                if (kingdom == "vertebrate_mammalian") {
                    # filter for kingdom before groups are determined
                    listgenomes.data <-
                        dplyr::filter(
                            listgenomes.data,
                            kingdoms == "Eukaryota",
                            group.y == "Animals",
                            subgroup == "Mammals"
                        )
                }
                
                if (kingdom == "invertebrate") {
                    # filter for kingdom before groups are determined
                    listgenomes.data <-
                        dplyr::filter(
                            listgenomes.data,
                            kingdoms == "Eukaryota",
                            group.y == "Animals",
                            subgroup %in% c(
                                "Insects",
                                "Other Animals",
                                "Roundworms",
                                "Flatworms"
                            )
                        )
                }
                
                if (kingdom == "vertebrate_other") {
                    # filter for kingdom before groups are determined
                    listgenomes.data <-
                        dplyr::filter(
                            listgenomes.data,
                            kingdoms == "Eukaryota",
                            group.y == "Animals",
                            subgroup %in% c("Amphibians", "Birds", "Fishes", 
                                            "Reptiles")
                        )
                }
            }
            # case Plants
            if (is.element(kingdom, c("plant"))) {
                # filter for kingdom before groups are determined
                listgenomes.data <-
                    dplyr::filter(listgenomes.data,
                                  kingdoms == "Eukaryota",
                                  group.y == "Plants")
            }
            # case Fungi
            if (is.element(kingdom, c("fungi"))) {
                # filter for kingdom before groups are determined
                listgenomes.data <-
                    dplyr::filter(listgenomes.data,
                                  kingdoms == "Eukaryota",
                                  group.y == "Fungi")
            }
            # case Protozoa
            if (is.element(kingdom, c("protozoa"))) {
                # filter for kingdom before groups are determined
                listgenomes.data <-
                    dplyr::filter(listgenomes.data,
                                  kingdoms == "Eukaryota",
                                  group.y == "Protists")
            }
            # case Viruses
            if (is.element(kingdom, c("viral"))) {
                # filter for kingdom before groups are determined
                listgenomes.data <-
                    dplyr::filter(listgenomes.data,
                                  kingdoms %in% c("Viruses", "Viroids"))
            }
            # case Bacteria
            if (is.element(kingdom, c("bacteria"))) {
                # filter for kingdom before groups are determined
                listgenomes.data <-
                    dplyr::filter(listgenomes.data, kingdoms == "Bacteria")
            }
            # case Archaea
            if (is.element(kingdom, c("archaea"))) {
                # filter for kingdom before groups are determined
                listgenomes.data <-
                    dplyr::filter(listgenomes.data, kingdoms == "Archaea")
            }
            
            # change only first character to upper upper case
            #kingdom <- toupper_first_char(kingdom)
            
            if (nrow(listgenomes.data) == 0)
                stop("Unfortunately, no group is available for ",
                     kingdom,
                     ".",
                     call. = FALSE)
        }
        
        uniq.species <-
            dplyr::summarise(
                dplyr::group_by(listgenomes.data, organism_name, subgroup),
                unique_elements = dplyr::n_distinct(organism_name)
            )
        
        if (!details) {
            return(table(uniq.species$subgroup))
        } else {
            return(uniq.species)
        }
    }
