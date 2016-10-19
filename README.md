biomartr
========

## Genomic Data Retrieval with R

The vastly growing number of sequenced genomes allows us to perform a new type of biological research.
Using a comparative approach these genomes provide us with new insights on how biological information is encoded 
on the molecular level and how this information changes over evolutionary time.

The first step, however, of any genome based study is to retrieve genomes from databases. For automating this
retrieval process on a meta-genomic scale, the `biomartr` package provides useful interface functions for genomic sequence retrieval and functional annotation retrieval. The major aim of `biomartr` is to facilitate reproducibility and large-scale handling of genomic data for (meta-)genomic analyses.  

In detail, `biomartr` aims to provide users with an easy to use framework to obtain genome, proteome, and CDS data, as well as an interface to the [BioMart](http://www.biomart.org/) database to retrieve functional annotation for genomic loci.
Hence, the `biomartr` package is designed to achieve the highest degree of reproducible research from data retrieval to data visualization.

Additionally, the `biomartr` package allows users to retrieve entire NCBI databases using only one command (see [Database Retrieval Vignette](https://github.com/HajkD/biomartr/blob/master/vignettes/Database_Retrieval.Rmd)).

## Tutorials

Getting Started with `biomartr`:

- [Introduction](https://github.com/HajkD/biomartr/tree/master/vignettes/Introduction.Rmd)
- [Database Retrieval](https://github.com/HajkD/biomartr/blob/master/vignettes/Database_Retrieval.Rmd)
- [Sequence and Database Retrieval](https://github.com/HajkD/biomartr/tree/master/vignettes/Sequence_Retrieval.Rmd)
- [Meta-Genome Retrieval](https://github.com/HajkD/biomartr/tree/master/vignettes/MetaGenome_Retrieval.Rmd)
- [Functional Annotation](https://github.com/HajkD/biomartr/tree/master/vignettes/Functional_Annotation.Rmd)
- [Evolutionary Transcriptomics using myTAI, orthologr, and biomartr](https://github.com/HajkD/biomartr/tree/master/vignettes/Evolutionary_Transcriptomics.Rmd)


## Installation

Before users can download and install `biomartr` they need to install the following packages from [Bioconductor](http://www.bioconductor.org/):

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

Users might be asked during the installation process of `Biostrings` and `biomaRt` whether or not they would like to update all package dependencies of the corresponding packages.
Please type `a` specifying that all package dependencies of the corresponding packages shall be updated. This is important for the sufficient functionality of `biomartr`.

Now users can download `biomartr` from CRAN :

```r
# install biomartr 0.1.0 from CRAN
install.packages("biomartr",
                 repos        = "https://cran.rstudio.com/",
                 dependencies = TRUE)
```

## NEWS
The current status of the package as well as a detailed history of the functionality of each version of `biomartr` can be found in the [NEWS](https://github.com/HajkD/biomartr/blob/master/NEWS.md) section.

## Genomic Data Retrieval

#### Meta-Genome Retrieval

* `meta.retieval()` : Perform Meta-Genome Retieval from NCBI of species belonging to the same kingdom of life 
* `meta.retieval.all()` : Perform Meta-Genome Retieval from NCBI of the entire kingdom of life
* `getMetaGenomes()` : Retrieve metagenomes from NCBI Genbank
* `getMetaGenomeAnnotations()` : Retrieve annotation *.gff files for metagenomes from NCBI Genbank
* `listMetaGenomes()` : List available metagenomes on NCBI Genbank
* `getMetaGenomeSummary()` : Helper function to retrieve the assembly_summary.txt file from NCBI genbank metagenomes

#### Genome Retrieval

* `getGenome()` : Download a specific genome stored on the NCBI ftp:// server
* `listGenomes()` : List all genomes available on the NCBI ftp:// server
* `is.genome.available()` : Check Genome Availability
* `getProteome()` : Download a specific proteome stored on the NCBI ftp:// server
* `getCDS()` : Download a specific CDS file (genome) stored on the NCBI ftp:// server
* `getKingdomAssemblySummary()` : Helper function to retrieve the assembly_summary.txt files from NCBI for all kingdoms
* `getMetaGenomeSummary()` : Helper function to retrieve the assembly_summary.txt files from NCBI genbank metagenomes
* `getSummaryFile()` : Helper function to retrieve the assembly_summary.txt file from NCBI for a specific kingdom

#### Database Retrieval

* `listDatabases()` : Retrieve a list of available NCBI databases
* `download.database()` : Download a NCBI database to your local hard drive
* `download.database.all()` : Download a complete NCBI Database such as e.g. `NCBI nr` to your local hard drive

### BioMart Queries

* `biomart()` : Main function to query the BioMart database
* `getMarts()` : Retrieve All Available BioMart Databases
* `getDatasets()` : Retrieve All Available Datasets for a BioMart Database
* `getAttributes()` : Retrieve All Available Attributes for a Specific Dataset
* `getFilters()` : Retrieve All Available Filters for a Specific Dataset
* `organismBM()` : Function for organism specific retrieval of available BioMart marts and datasets
* `organismAttributes()` : Function for organism specific retrieval of available BioMart attributes
* `organismFilters()` : Function for organism specific retrieval of available BioMart filters

### Performing Gene Ontology queries

#### Gene Ontology

* `getGO()` : Function to retrieve GO terms for a given set of genes


## Download Developer Version

The developer version of `biomartr` might include more functionality than the stable version on CRAN.

### On Unix Based Systems

Now you can use the `devtools` package to install biomartr from GitHub.

```r
# install.packages("devtools")

# install the current version of biomartr on your system
library(devtools)
install_github("HajkD/biomartr", build_vignettes = TRUE, dependencies = TRUE)

```

### On Windows Systems

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

### Troubleshooting on Windows Machines

- Install `biomartr` on a Win 8 laptop: [solution](https://github.com/HajkD/orthologr/issues/1) ( Thanks to Andres Romanowski )

## Discussions and Bug Reports

I would be very happy to learn more about potential improvements of the concepts and functions
provided in this package.

Furthermore, in case you find some bugs or need additional (more flexible) functionality of parts
of this package, please let me know:

[twitter: HajkDrost](https://twitter.com/hajkdrost) or  [email](hgd23@cam.ac.uk)

For Bug Report: Please send me an [issue](https://github.com/HajkD/biomartr/issues).


