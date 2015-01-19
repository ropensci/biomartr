biomartr
========

## A Framework for Biological Data Retrieval

The `biomartr` package aims to provide users with an easy to use framework to obtain genome, proteome, and CDS data, as well as an interface to the [biomaRt](http://www.bioconductor.org/packages/release/bioc/html/biomaRt.html) functionality and [Gene Ontology](http://geneontology.org/page/go-enrichment-analysis)/[Kegg](http://www.genome.jp/kegg/) analyses. Furthermore, it was specifically designed to serve as additional module to
the [myTAI](https://github.com/HajkD/myTAI) and [orthologr](https://github.com/HajkD/orthologr) frameworks.


## Fast Installation Guide

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

## Tutorials

The following tutorials will introduce you to the
functionality of `biomartr` and how you can use it to extend
analyses implemented in [myTAI](http://cran.r-project.org/web/packages/myTAI/index.html) and [orthologr](https://github.com/HajkD/orthologr).

- [Introduction](https://github.com/HajkD/biomartr/tree/master/vignettes/Introduction.Rmd)
- [Sequence and Database Retrieval](https://github.com/HajkD/biomartr/tree/master/vignettes/Sequence_Retrieval.Rmd)
- [Functional Annotation](https://github.com/HajkD/biomartr/tree/master/vignettes/Functional_Annotation.Rmd)
- [Taxonomic Information](https://github.com/HajkD/biomartr/tree/master/vignettes/Taxonomy.Rmd)
- [Genomic Sequence Analyses and Statistics](https://github.com/HajkD/biomartr/tree/master/vignettes/Statistics.Rmd)
- [Phylotranscriptomics using myTAI, orthologr, and biomartr](https://github.com/HajkD/biomartr/tree/master/vignettes/Phylotranscriptomics.Rmd)


### The Following Functions Have Been Implemented for Biomart Query, as Well as Genome, Proteome, and CDS Retrieval

### BioMart Queries

* `biomart()` : Main function to query the BioMart database
* `organismBM()` : Function for organism specific retrieval of available BioMart marts and datasets
* `organismAttributes()` : Function for organism specific retrieval of available BioMart attributes
* `organismFilters()` : Function for organism specific retrieval of available BioMart filters
* `getMarts()` : Retrieve All Available BioMart Databases

### Biological Data Retrieval

#### Genome Retrieval

* `getGenome()` : A function for downloading a specific genome stored on the NCBI ftp:// server
* `listGenomes()` : Function to list all genomes available on the NCBI ftp:// server
* `geneSequence()` : A function to retrieve biological sequences of a given set of genes

#### Proteome Retrieval

* `getProteome()` : A function for downloading a specific proteome stored on the NCBI ftp:// server

#### CDS Retrieval

* `getCDS()` : A function for downloading a specific CDS file (genome) stored on the NCBI ftp:// server

#### Database Retrieval

* `listDatabases()` : Retrieve a List of Available Databases for Download
* `download_database()` : Download a Database to Your Local Hard Drive

#### Taxonomic Information

* `taxonomy()` : Retrieving Taxonomic Information of a Query Organism

### Performing Gene Ontology queries

#### Gene Ontology

* `getGO()` : Function to retrieve GO terms for a given set of genes

#### KeGG Ontology

* `getKegg()` : Function to retrieve Kegg annotation for a given set of genes

### Statistics

#### Alignment Statistics

* `randomSeqs()` : Random sequence generator based on a multinomial model
* `randSeqDistr()` : Function to generate a distribution of alignment scores based on random sequences
* `evalAlignment()` : Function to quantify the statistical significance of a given pairwise alignment

### Visualization

* `visGenome()` : Visualization function for genome properties

## Discussions and Bug Reports

I would be very happy to learn more about potential improvements of the concepts and functions
provided in this package.

Furthermore, in case you find some bugs or need additional (more flexible) functionality of parts
of this package, please let me know:

hajk-georg.drost@informatik.uni-halle.de


## Acknowledgement

I would like to thank Ivo Grosse and Marcel Quint for providing me a place and the environment to be able to work on 
fascinating topics in Theoretical Biology.
