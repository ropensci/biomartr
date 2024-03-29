% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSet.R
\name{getCDSSet}
\alias{getCDSSet}
\title{CDS retrieval of multiple species}
\usage{
getCDSSet(
  db = "refseq",
  organisms,
  reference = FALSE,
  release = NULL,
  gunzip = TRUE,
  update = FALSE,
  path = "set_CDS"
)
}
\arguments{
\item{db}{a character string specifying the database from which the genome
shall be retrieved:
\itemize{
\item \code{db = "refseq"}
\item \code{db = "genbank"}
\item \code{db = "ensembl"}
}}

\item{organisms}{a character vector storing the names of the organisms than shall be retrieved.
There are three available options to characterize an organism:}

\item{reference}{a logical value indicating whether or not a genome shall be downloaded if it isn't marked in the database as either a reference genome or a representative genome.}

\item{release}{a numeric, the database release version of ENSEMBL (\code{db = "ensembl"}). Default is \code{release = NULL} meaning
that the most recent database version is used. \code{release = 75} would for human would give the stable
GRCh37 release in ensembl. Value must be > 46, since ensembl did not structure their data
if the standard format before that.}

\item{gunzip}{a logical, indicating whether or not files should be unzipped.}

\item{update}{logical, default FALSE. Updated backend cached files needed.
Usually keep this false, to make ut run much faster. Only set to TRUE, if you
believe you cache is outdated (Species only exist in newest release etc)}

\item{path}{character, default location is paste0("set_", toupper(set_type))}
}
\value{
File path to downloaded genomes
(names are identifiers: 'new' (file was downloaded now),
'old' files did already exist)
}
\description{
Main CDS retrieval function for a set of organism of interest.
By specifying the scientific names of the organisms of interest the corresponding fasta-files storing the CDS of the organisms of interest
will be downloaded and stored locally. CDS files can be retrieved from several databases.
}
\details{
Internally this function loads the the overview.txt file from NCBI:

 refseq: ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/

 genbank: ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/

and creates a directory 'set_CDSs' to store
the CDSs of interest as fasta files for future processing.
In case the corresponding fasta file already exists within the
'set_CDSs' folder and is accessible within the workspace,
no download process will be performed.
}
\examples{
\dontrun{
getBioSet("refseq", organisms = c("Arabidopsis thaliana",
                                  "Arabidopsis lyrata",
                                  "Capsella rubella"),
                                  set_type = "cds")
}
}
\seealso{
Other getBioSet: 
\code{\link{getBioSet}()},
\code{\link{getCollectionSet}()},
\code{\link{getGFFSet}()},
\code{\link{getGenomeSet}()},
\code{\link{getProteomeSet}()},
\code{\link{getRNASet}()}

Other cds: 
\code{\link{getCDS}()},
\code{\link{read_cds}()}
}
\author{
Hajk-Georg Drost
}
\concept{cds}
\concept{getBioSet}
