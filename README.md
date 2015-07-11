biomartr
========

## Biological Data Retrieval using R

The `biomartr` package aims to provide users with an easy to use framework to obtain genome, proteome, and CDS data, as well as an interface to the [BioMart](http://www.biomart.org/) and [DAVID](http://david.abcc.ncifcrf.gov/) to retrieve functional annotation from [Gene Ontology](http://geneontology.org/page/go-enrichment-analysis) and [Kegg](http://www.genome.jp/kegg/). Furthermore, it is specifically designed to serve as additional module to
the [myTAI](https://github.com/HajkD/myTAI) and [orthologr](https://github.com/HajkD/orthologr) frameworks, allowing the highest degree of reproducibility in phylotranscriptomics research from data retrieval to data visualization.


## Installation Guide

Before you can load and install orthologr you need to install the following packages from [Bioconductor](http://www.bioconductor.org/):

```r
# install Bioconductor base packages
source("http://bioconductor.org/biocLite.R")
biocLite()

# load the biomaRt package
source("http://bioconductor.org/biocLite.R")
biocLite("biomaRt")

# load the Biostrings package
source("http://bioconductor.org/biocLite.R")
biocLite("Biostrings")
```

## On Unix Based Systems

Now you can use the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package to install orthologr from GitHub.

```r
# install.packages("devtools")

# install the current version of biomartr on your system
library(devtools)
install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

```

## On Windows Systems

```r
# On Windows, this won't work - see ?build_github_devtools
install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

# When working with Windows, first you need to install the
# R package: rtools -> install.packages("rtools")

# Afterwards you can install devtools -> install.packages("devtools")
# and then you can run:

devtools::install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

# and then call it from the library
library("biomartr", lib.loc = "C:/Program Files/R/R-3.1.1/library")
```

## Tutorials

The following tutorials will introduce you to the
functionality of `biomartr` and how you can use it to extend
analyses implemented in [myTAI](http://cran.r-project.org/web/packages/myTAI/index.html) and [orthologr](https://github.com/HajkD/orthologr).

- [Introduction](https://github.com/HajkD/biomartr/tree/master/vignettes/Introduction.Rmd)
- [Sequence and Database Retrieval](https://github.com/HajkD/biomartr/tree/master/vignettes/Sequence_Retrieval.Rmd)
- [Functional Annotation](https://github.com/HajkD/biomartr/tree/master/vignettes/Functional_Annotation.Rmd)
- [Taxonomic Information](https://github.com/HajkD/biomartr/tree/master/vignettes/Taxonomy.Rmd)
- [Phylotranscriptomics using myTAI, orthologr, and biomartr](https://github.com/HajkD/biomartr/tree/master/vignettes/Phylotranscriptomics.Rmd)


## Functionality

### BioMart Queries

* `biomart()` : Main function to query the BioMart database
* `getMarts()` : Retrieve All Available BioMart Databases
* `getDatasets()` : Retrieve All Available Datasets for a BioMart Database
* `getAttributes()` : Retrieve All Available Attributes for a Specific Dataset
* `getFilters()` : Retrieve All Available Filters for a Specific Dataset
* `organismBM()` : Function for organism specific retrieval of available BioMart marts and datasets
* `organismAttributes()` : Function for organism specific retrieval of available BioMart attributes
* `organismFilters()` : Function for organism specific retrieval of available BioMart filters


### Biological Data Retrieval

#### Genome Retrieval

* `getGenome()` : A function for downloading a specific genome stored on the NCBI ftp:// server
* `listGenomes()` : Function to list all genomes available on the NCBI ftp:// server
* `is.genome.available()` : Check Genome Availability
#### Proteome Retrieval

* `getProteome()` : A function for downloading a specific proteome stored on the NCBI ftp:// server

#### CDS Retrieval

* `getCDS()` : A function for downloading a specific CDS file (genome) stored on the NCBI ftp:// server

#### Database Retrieval

* `listDatabases()` : Retrieve a List of Available Databases for Download
* `download_database()` : Download a Database to Your Local Hard Drive
* `phytomine.organisms()` : Retrieve All Organism Names Stored on Phytozome v10
* `flymine.organisms()` : Retrieve All Organism Names Stored on FlyMine
* `zebrafishmine.organisms()` : Retrieve All Organism Names Stored on ZebrafishMine

### Performing Gene Ontology queries

#### Gene Ontology

* `getGO()` : Function to retrieve GO terms for a given set of genes

#### KeGG Ontology

* `getKegg()` : Function to retrieve Kegg annotation for a given set of genes

## Discussions and Bug Reports

I would be very happy to learn more about potential improvements of the concepts and functions
provided in this package.

Furthermore, in case you find some bugs or need additional (more flexible) functionality of parts
of this package, please let me know:

[twitter: HajkDrost](https://twitter.com/hajkdrost) or  [email](hajk-georg.drost@informatik.uni-halle.de)

For Bug Report: Please send me an [issue](https://github.com/HajkD/biomartr/issues).


