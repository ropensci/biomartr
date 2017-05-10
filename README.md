biomartr
========

[![Travis-CI Build Status](https://travis-ci.org/HajkD/biomartr.svg?branch=master)](https://travis-ci.org/HajkD/biomartr)

## Genomic Data Retrieval with R

### Motivation:

This package is born out of my own frustration to automate the genomic data retrieval process to create computationally reproducible scripts for large-scale genomics studies. Since I couldn't find easy-to-use and fully reproducible software libraries that would allow others and me to write transparent and easy to reproduce code, I sat down and tried to implement a framework that would enable anyone to automate the genomic data retrieval process. Personally, I strongly support and believe in reproducible research, and I truly hope that this package might be useful to others as well and that it helps to promote reproducible research in genomics studies.

This package is far from being perfect or finished. Currently, I am struggeling with the fact that the ENSEMBL and NCBI servers limit the number of queries which sometimes makes it hard to retrieve hundreds of genomes in one seamless run (see also issue https://github.com/HajkD/biomartr/issues/12). However, I am working on this issue and try to find a way around it and I hope that with the help of the community `biomartr` can grow to become a useful genomics tool and a basis for reproducible genomics studies.

I happily welcome anyone who wishes to contribute to this project :) Just drop me an email.

### Short package description:

The vastly growing number of sequenced genomes allows us to perform a new type of biological research.
Using a comparative approach these genomes provide us with new insights on how biological information is encoded 
on the molecular level and how this information changes over evolutionary time.

The first step, however, of any genome based study is to retrieve genomes from databases. To automate the
retrieval process on a meta-genomic scale, the `biomartr` package provides useful interface functions for genomic sequence retrieval and functional annotation retrieval. The major aim of `biomartr` is to facilitate computational reproducibility and large-scale handling of genomic data for (meta-)genomic analyses.  

In detail, `biomartr` aims to provide users with an easy to use framework to obtain genome, proteome, CDS, GFF (annotation), genome assembly quality, and metagenome project data. Furthermore, an interface to the [Ensembl Biomart](www.ensembl.org/biomart) database allows users to retrieve functional annotation for genomic loci.
Users can [download entire databases](https://github.com/HajkD/biomartr/blob/master/vignettes/Database_Retrieval.Rmd) such as `NCBI RefSeq`, `NCBI nr`, `NCBI nt`, `NCBI Genbank`, etc. as well as `ENSEMBL` and `ENSEMBLGENOMES` with only one command.

Hence, the `biomartr` package is designed to achieve the highest degree of computational reproducibility in genomics research.

### Citation

**Please cite the following paper when using `biomartr` for your own research. This will allow me to continue
working on this software tool and will motivate me to extend its functionality and usability in the next years. Many thanks in advance :)**

> Drost HG, Paszkowski J. __Biomartr: genomic data retrieval with R__. *Bioinformatics* (2017) 33(8): 1216-1217. [doi:10.1093/bioinformatics/btw821](https://academic.oup.com/bioinformatics/article/doi/10.1093/bioinformatics/btw821/2931816/Biomartr-genomic-data-retrieval-with-R).

### Platforms

> Find `biomartr` also at [OmicTools](https://omictools.com/biomartr-tool).

### Frequently Asked Questions (FAQs)

Please find [all FAQs here](FAQs.md).

## Discussions and Bug Reports

I would be very happy to learn more about potential improvements of the concepts and functions
provided in this package.

Furthermore, in case you find some bugs or need additional (more flexible) functionality of parts
of this package, please let me know:

[twitter: HajkDrost](https://twitter.com/hajkdrost) or  [email](hgd23@cam.ac.uk)

For Bug Reports: Please send me an [issue](https://github.com/HajkD/biomartr/issues).

## Tutorials

Getting Started with `biomartr`:

- [Introduction](https://github.com/HajkD/biomartr/tree/master/vignettes/Introduction.Rmd)
- [Database Retrieval](https://github.com/HajkD/biomartr/blob/master/vignettes/Database_Retrieval.Rmd)
- [Genomic Sequence Retrieval](https://github.com/HajkD/biomartr/tree/master/vignettes/Sequence_Retrieval.Rmd)
- [Meta-Genome Retrieval](https://github.com/HajkD/biomartr/tree/master/vignettes/MetaGenome_Retrieval.Rmd)
- [Functional Annotation](https://github.com/HajkD/biomartr/tree/master/vignettes/Functional_Annotation.Rmd)
- [BioMart Examples](https://github.com/HajkD/biomartr/tree/master/vignettes/BioMart_Examples.Rmd)


Users can also read the tutorials within ([RStudio](http://www.rstudio.com/)) :

```r
# source the biomartr package
library(biomartr)

# look for all tutorials (vignettes) available in the biomartr package
# this will open your web browser
browseVignettes("biomartr")
```

## Installation

```r
# install biomartr 0.4.0
source("http://bioconductor.org/biocLite.R")
biocLite('biomartr')
```

## Install Developer Version
Some bug fixes or new functionality will not be available on CRAN yet, but in the developer version here on GitHub. To download and install the most recent version of `biomartr` run:

```r
# install the current version of biomartr on your system
source("http://bioconductor.org/biocLite.R")
biocLite("HajkD/biomartr")
```


## NEWS
The current status of the package as well as a detailed history of the functionality of each version of `biomartr` can be found in the [NEWS](https://github.com/HajkD/biomartr/blob/master/NEWS.md) section.

## Genomic Data Retrieval

#### Meta-Genome Retrieval

* `meta.retrieval()` : Perform Meta-Genome Retieval from NCBI of species belonging to the same kingdom of life or to the same taxonomic subgroup
* `meta.retrieval.all()` : Perform Meta-Genome Retieval from NCBI of the entire kingdom of life
* `getMetaGenomes()` : Retrieve metagenomes from NCBI Genbank
* `getMetaGenomeAnnotations()` : Retrieve annotation *.gff files for metagenomes from NCBI Genbank
* `listMetaGenomes()` : List available metagenomes on NCBI Genbank
* `getMetaGenomeSummary()` : Helper function to retrieve the assembly_summary.txt file from NCBI genbank metagenomes

#### Genome Retrieval

* `listGenomes()` : List all genomes available on NCBI and ENSEMBL servers
* `listKingdoms()` : list the number of available species per kingdom of life on NCBI and ENSEMBL servers
* `listGroups()` : list the number of available species per group on NCBI and ENSEMBL servers
* `getKingdoms()` : Retrieve available kingdoms of life
* `getGroups()` : Retrieve available groups for a kingdom of life
* `is.genome.available()` : Check Genome Availability  NCBI and ENSEMBL servers
* `getGenome()` : Download a specific genome stored on NCBI and ENSEMBL servers
* `getProteome()` : Download a specific proteome stored on NCBI and ENSEMBL servers
* `getCDS()` : Download a specific CDS file (genome) stored on NCBI and ENSEMBL servers
* `getRNA()` : Download a specific RNA file stored on NCBI and ENSEMBL servers
* `getGFF()` : Genome Annotation Retrieval from NCBI (`*.gff`) and ENSEMBL (`*.gff3`) servers
* `getGTF()` : Genome Annotation Retrieval (`*.gtf`) from ENSEMBL servers
* `getAssemblyStats()` : Genome Assembly Stats Retrieval from NCBI
* `getKingdomAssemblySummary()` : Helper function to retrieve the assembly_summary.txt files from NCBI for all kingdoms
* `getMetaGenomeSummary()` : Helper function to retrieve the assembly_summary.txt files from NCBI genbank metagenomes
* `getSummaryFile()` : Helper function to retrieve the assembly_summary.txt file from NCBI for a specific kingdom
* `getENSEMBLInfo()` : Retrieve ENSEMBL info file
* `getGENOMEREPORT()` : Retrieve GENOME_REPORTS file from NCBI

#### Import Downloaded Files
* `read_genome()` : Import genomes as Biostrings or data.table object
* `read_proteome()` : Import proteome as Biostrings or data.table object
* `read_cds()` : Import CDS as Biostrings or data.table object
* `read_gff()` : Import GFF file
* `read_rna()` : Import RNA file
* `read_assemblystats()` : Import Genome Assembly Stats File

#### Database Retrieval

* `listNCBIDatabases()` : Retrieve a List of Available NCBI Databases for Download
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


## Download Developer Version On Windows Systems

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

# Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.


