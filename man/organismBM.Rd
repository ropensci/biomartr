% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/organismBM.R
\name{organismBM}
\alias{organismBM}
\title{Retrieve Ensembl Biomart marts and datasets for a query organism}
\usage{
organismBM(organism = NULL, update = FALSE, mute_citation = TRUE)
}
\arguments{
\item{organism}{a character string specifying the scientific name of a
query organism. Default is \code{organism} = \code{NULL}. In this case all
available biomart connections are returned.}

\item{update}{a logical value specifying whether or not the local
listMart.txt and listDatasets.txt files shall be updated by remote access
 to BioMart.}

\item{mute_citation}{logical value indicating whether citation message should be muted.}
}
\description{
This function returns either all available biomart connections
for all available organisms for which biomart access is possible, or
(when specified) returns all organism specific biomart connections.
}
\details{
This function collects all available biomart connections and returns a table
storing the organism for which biomart connections are available as well as
the corresponding mart and database.
}
\note{
When you run this function for the first time, the data retrieval procedure
will take some time, due to the remote access to BioMart. The corresponding
result is then saved in a *.txt file named "_biomart/listDatasets.txt" in the
\code{\link{tempdir}} directory, allowing subsequent queries to perform
 much faster.
}
\examples{
\dontrun{
# returning all available biomart connections
head(organismBM(), 20)
# retrieving all available datasets and biomart connections for
# a specific query organism (scientific name)
organismBM(organism = "Homo sapiens")
# you can also update the downloaded version using
# the "update = TRUE" argument
head(organismBM(update = TRUE), 20)
}
}
\references{
\url{https://biomart.org/}

Mapping identifiers for the integration of genomic datasets with the
R/Bioconductor package biomaRt. Steffen Durinck, Paul T. Spellman, Ewan
Birney and Wolfgang Huber, Nature Protocols 4, 1184-1191 (2009).

BioMart and Bioconductor: a powerful link between biological databases and
microarray data analysis. Steffen Durinck, Yves Moreau, Arek Kasprzyk, Sean
Davis, Bart De Moor, Alvis Brazma and Wolfgang Huber, Bioinformatics 21,
3439-3440 (2005).
}
\seealso{
Other biomaRt: 
\code{\link{biomart}()},
\code{\link{getAttributes}()},
\code{\link{getDatasets}()},
\code{\link{getMarts}()},
\code{\link{organismFilters}()}
}
\author{
Hajk-Georg Drost
}
\concept{biomaRt}
