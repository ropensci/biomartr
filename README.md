biomartr
========

The `biomartr` package aims to provide users of the [myTAI](https://github.com/HajkD/myTAI) and [orthologr](https://github.com/HajkD/orthologr) packages with an easy to use framework to obtain genome, proteome, and CDS data, as well as an interface to the [biomaRt](http://www.bioconductor.org/packages/release/bioc/html/biomaRt.html) functionality and [Gene Ontology](http://geneontology.org/page/go-enrichment-analysis)/[Kegg](http://www.genome.jp/kegg/) analyses.


## Fast installation guide

```r
# install.packages("devtools")

# install the current version of biomartr on your system
library(devtools)
install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

# On Windows, this won't work - see ?build_github_devtools
install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

# When working with Windows, first you need to install the
# R package: rtools -> install.packages("rtools")

# Afterwards you can install devtools -> install.packages("devtools")
# and then you can run:

devtools::install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

# and then call it from the library
library("biomartr", lib.loc = "C:/Program Files/R/R-3.1.1/library")


# additionally load: biomaRt
source("http://bioconductor.org/biocLite.R")
biocLite("biomaRt")

```

### The following functions have been implemented for genome, proteome, and CDS retrieval

* `getGenome()` : A function for downloading a specific genome stored on the NCBI ftp:// server
* `listGenomes()` : Function to list all genomes available on the NCBI ftp:// server
* `geneSequence()` : A function to retrieve biological sequences of a given set of genes

### Interface to biomart

* `biomart()` : Main function to query the biomart database

### Performing Gene Ontology queries

* `getGO()` : Function to retrieve GO terms for a given set of genes


## Discussions and Bug Reports

I would be very happy to learn more about potential improvements of the concepts and functions
provided in this package.

Furthermore, in case you find some bugs or need additional (more flexible) functionality of parts
of this package, please let me know:

hajk-georg.drost@informatik.uni-halle.de




