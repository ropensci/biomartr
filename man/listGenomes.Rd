% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listGenomes.R
\name{listGenomes}
\alias{listGenomes}
\title{List All Available Genomes either by kingdom, group, or subgroup}
\usage{
listGenomes(
  db = "refseq",
  type = "all",
  subset = NULL,
  details = FALSE,
  update = FALSE,
  skip_bacteria = FALSE
)
}
\arguments{
\item{db}{a character string specifying the database for which genome
availability shall be checked. Available options are:
\itemize{
\item \code{db = "refseq"}
\item \code{db = "genbank"}
\item \code{db = "ensembl"}
}}

\item{type}{a character string specifying a potential filter of available
genomes. Available options are:
\itemize{
\item \code{type = "all", no subset}
\item \code{type = "kingdom", subset on kingdom}
\item \code{type = "group", subset on group}
\item \code{type = "subgroup", subset on subgroup}
}}

\item{subset}{a character string or character vector specifying a subset of
\code{type}. E.g. if users are interested in retrieving all
\code{Eukaryota} species, they can specify: \code{type = "kingdom"} and
\code{subset = "Eukaryota"}.}

\item{details}{a boolean value specifying whether only the scientific names
of stored genomes shall be returned (details = FALSE) or all information such
as
\itemize{
\item \code{organism_name}
\item \code{kingdoms}
\item \code{group}
\item \code{subgroup}
\item \code{file_size_MB}, etc.
}}

\item{update}{logical, default FALSE. If TRUE, update cached list,
if FALSE use existing cache (if it exists). For cache location see
\code{cachedir()}}

\item{skip_bacteria}{Due to its enormous dataset size (> 700MB as of July 2023),
the bacterial summary file will not be loaded by default anymore. If users
wish to gain insights for the bacterial kingdom they needs to actively specify \code{skip_bacteria = FALSE}. When \code{skip_bacteria = FALSE} is set then the
bacterial summary file will be downloaded.}
}
\description{
This function retrieves the names of all genomes available on
the NCBI ftp:// server and stores the results in a file named 'overview.txt'
inside the directory _ncbi_downloads' that is built inside the workspace.
}
\details{
Internally this function loads the the overview.txt file from NCBI
and creates a directory '_ncbi_downloads' in the \code{temdir()}
folder to store the overview.txt file for future processing. In case the
overview.txt file already exists within the '_ncbi_downloads' folder and is
accessible within the workspace, no download process will be performed again.
}
\note{
Please note that the ftp:// connection relies on the NCBI or ENSEMBL server
and cannot be accurately accessed via a proxy.
}
\examples{
\dontrun{
# print details for refseq
listGenomes(db = "refseq")
# print details for all plants in refseq
listGenomes(db = "refseq", type = "kingdom")
# print details for all plant groups in refseq
listGenomes(db = "refseq", type = "group")
# print details for all plant subgroups in refseq
listGenomes(db = "refseq", type = "subgroup")
# Ensembl
listGenomes(db = "ensembl", type = "kingdom", subset = "EnsemblVertebrates")
}
}
\author{
Hajk-Georg Drost
}
