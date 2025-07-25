% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/organismAttributes.R
\name{organismAttributes}
\alias{organismAttributes}
\title{Retrieve Ensembl Biomart attributes for a query organism}
\usage{
organismAttributes(organism, update = FALSE, topic = NULL)
}
\arguments{
\item{organism}{a character string specifying the scientific name 
of a query organism.}

\item{update}{a logical value specifying whether or not the local 
listMart.txt, listDatasets.txt, and listAttributes_organism.txt files shall 
be updated by remote access to BioMart.}

\item{topic}{a character string specifying a topic (category) of attributes, 
e.g. \code{topic} = \code{"id"}.}
}
\value{
a data.frame storing corresponding attribute names, description, 
datasets, and marts.
}
\description{
In addition to the \code{\link{organismBM}} function, 
this function returns all available attributes that can be accessed through 
different marts and datasets for a given query organism.
}
\details{
For a given query organism, this function retrieves all available attributes 
that can be accessed through different marts and datasets.

Sometimes the same attribute names correspond to different datasets and 
marts causing problems when using \code{\link{getMarts}}. The approach 
introduced by this function provides (again) a organism centric way of 
accessing organism specific attributes.

The \code{topic} argument allows the user to search for specific attribute 
topics/categories for faster filtering.
}
\note{
When you run this function for the first time, the data retrieval procedure 
will take some time, due to the remote access to BioMart. The corresponding 
result is then saved in a *.txt file within the \code{\link{tempdir}}
directory named "_biomart/listMarts.txt","_biomart/listDatasets.txt", and 
"_biomart/listAttributes_organism.txt", allowing subsequent queries to 
perform much faster.
}
\examples{
\dontrun{ 
# search for attribute topic id
head(organismAttributes("Homo sapiens", topic = "id"), 20)
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
\code{\link{organismFilters}}, \code{\link{organismBM}}, 
\code{\link{biomart}}, \code{\link[biomaRt]{listAttributes}}
}
\author{
Hajk-Georg Drost
}
