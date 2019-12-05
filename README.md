biomartr
========
[![](https://badges.ropensci.org/93_status.svg)](https://github.com/ropensci/onboarding/issues/93)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/biomartr)](https://github.com/metacran/cranlogs.app)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/biomartr)](https://github.com/metacran/cranlogs.app)
 
 
## Genomic Data Retrieval with R

### Motivation:

This package is born out of my own frustration to automate the genomic data retrieval process to create computationally reproducible scripts for large-scale genomics studies. Since I couldn't find easy-to-use and fully reproducible software libraries I sat down and tried to implement a framework that would enable anyone to automate and standardize the genomic data retrieval process. I hope that this package is useful to others as well and that it helps to promote reproducible research in genomics studies.

I happily welcome anyone who wishes to contribute to this project :) Just drop me an email.

Please find a detailed [documentation here](https://docs.ropensci.org/biomartr/articles/).


### Citation

**I develop `biomartr` in my spare time and would be very grateful if you could cite the following paper in case `biomartr` was useful for your own research. I plan on vastly extending the biomartr functionality and usability in the next years to facilitate reproducible genomics research and require citations to back up these efforts. Many thanks in advance :)**

> Drost HG, Paszkowski J. __Biomartr: genomic data retrieval with R__. *Bioinformatics* (2017) 33(8): 1216-1217. [doi:10.1093/bioinformatics/btw821](https://academic.oup.com/bioinformatics/article/doi/10.1093/bioinformatics/btw821/2931816/Biomartr-genomic-data-retrieval-with-R).


### Short package description:

The vastly growing number of sequenced genomes allows us to perform a new type of biological research.
Using a comparative approach these genomes provide us with new insights on how biological information is encoded 
on the molecular level and how this information changes over evolutionary time.

The first step, however, of any genome based study is to retrieve genomes and their annotation from databases. To automate the
retrieval process of this information on a meta-genomic scale, the `biomartr` package provides interface functions for genomic sequence retrieval and functional annotation retrieval. The major aim of `biomartr` is to facilitate computational reproducibility and large-scale handling of genomic data for (meta-)genomic analyses.
In addition, `biomartr` aims to address the `genome version crisis`. With `biomartr` users can now control and be informed 
about the genome versions they retrieve automatically. Many large scale genomics studies lack this information
and thus, reproducibility and data interpretation become nearly impossible when documentation of genome version information
gets neglected.

In detail, `biomartr` automates genome, proteome, CDS, RNA, Repeats, GFF/GTF (annotation), genome assembly quality, and metagenome project data retrieval from the major biological databases such as

- [NCBI RefSeq](https://www.ncbi.nlm.nih.gov/refseq/)
- [NCBI Genbank](https://www.ncbi.nlm.nih.gov/genbank/)
- [ENSEMBL](https://www.ensembl.org/index.html)
- [ENSEMBLGENOMES](http://ensemblgenomes.org) (as of April 2019 - `ENSEMBL` and `ENSEMBLGENOMES` were joined - see [details here](http://www.ensembl.info/2019/03/08/joint-rest-server-for-ensembl-and-ensembl-genomes-in-ensembl-96/))
- [UniProt](http://www.uniprot.org)

Furthermore, an interface to the [Ensembl Biomart](www.ensembl.org/biomart) database allows users to retrieve functional annotation for genomic loci using a novel and organism centric search strategy. In addition, users can [download entire databases](https://github.com/HajkD/biomartr/blob/master/vignettes/Database_Retrieval.Rmd) such as 

- `NCBI RefSeq` 
- `NCBI nr` 
- `NCBI nt`
- `NCBI Genbank`
- `ENSEMBL` 

with only one command.

### Similar Work

The main difference between the [BiomaRt](http://www.bioconductor.org/packages/release/bioc/html/biomaRt.html) package and the [biomartr](https://docs.ropensci.org/biomartr/) package is that `biomartr` extends the `functional annotation retrieval` procedure of `BiomaRt` and __in addition__ provides useful retrieval functions for genomes, proteomes, coding sequences, gff files, RNA sequences, Repeat Masker annotations files, and functions for the retrieval of entire databases such as `NCBI nr` etc.

Please consult the [Tutorials section](https://docs.ropensci.org/biomartr/#tutorials) for more details.

`In the context of functional annotation retrieval` the `biomartr` package allows users to screen available marts using only the scientific name of an organism of interest instead of first searching for marts and datasets which support a particular organism of interest (which is required when using the `BiomaRt` package). Furthermore, `biomartr` allows you to search for particular topics when searching for attributes and filters. I am aware that the similar naming of the packages is unfortunate, but it arose due to historical reasons (please find a detailed explanation here: https://github.com/ropensci/biomartr/blob/master/FAQs.md and here [#11](https://github.com/ropensci/biomartr/issues/11)).

I also dedicated [an entire vignette to compare](https://docs.ropensci.org/biomartr/articles/Functional_Annotation.html) the `BiomaRt` and `biomartr` package functionality in the context of `Functional Annotation` (where their functionality overlaps which comprises about only 20% of the overall functionality of the biomartr package).

### Feedback
>__I truly value your opinion and improvement suggestions. Hence, I would be extremely grateful if you could take this 1 minute and 3 question survey (https://goo.gl/forms/Qaoxxjb1EnNSLpM02) so that I can learn how to improve `biomartr` in the best possible way. Many many thanks in advance.__


## Installation

The `biomartr` package relies on some [Bioconductor](https://www.bioconductor.org/install/) tools and thus requires
installation of the following packages:

```r
# Install core Bioconductor packages
if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install()
# Install package dependencies
BiocManager::install("Biostrings", version = "3.8")
BiocManager::install("biomaRt", version = "3.8")

```

Now users can install `biomartr` from CRAN:

```r
# install biomartr 0.9.0
install.packages("biomartr", dependencies = TRUE)
```

## Example

### Collection Retrieval

The automated retrieval of collections (= Genome, Proteome, CDS, RNA, GFF, Repeat Masker, AssemblyStats files)
will make sure that the genome file of an organism will match the CDS, proteome, RNA, GFF, etc file
and was generated using the same genome assembly version. One aspect of why genomics studies
fail in computational and biological reproducibility is that it is not clear whether CDS, proteome, RNA, GFF, etc files
used in a proposed analysis were generated using the same genome assembly file denoting the same genome assembly version.
To avoid this seemingly trivial mistake we encourage users to retrieve
genome file collections using the `biomartr` function `getCollection()`
and attach the corresponding output as Supplementary Data
to the respective genomics study to ensure computational and biological reproducibility.


```r
# download collection for Saccharomyces cerevisiae
biomartr::getCollection( db = "refseq", organism = "Saccharomyces cerevisiae")
```

Internally, the `getCollection()` function will now generate a folder named `refseq/Collection/Saccharomyces_cerevisiae`
and will store all genome and annotation files for `Saccharomyces cerevisiae` in the same folder.
In addition, the exact genoem and annotation version will be logged in the `doc` folder.

Internally, a text file named `doc_Saccharomyces_cerevisiae_db_refseq.txt` is generated. The information stored in this log file is structured as follows:

```
File Name: Saccharomyces_cerevisiae_assembly_stats_refseq.txt
Organism Name: Saccharomyces_cerevisiae
Database: NCBI refseq
URL: ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/146/045/GCF_000146045.2_R64/GCF_000146045.2_R64_assembly_stats.txt
Download_Date: Wed Jun 27 15:21:51 2018
refseq_category: reference genome
assembly_accession: GCF_000146045.2
bioproject: PRJNA128
biosample: NA
taxid: 559292
infraspecific_name: strain=S288C
version_status: latest
release_type: Major
genome_rep: Full
seq_rel_date: 2014-12-17
submitter: Saccharomyces Genome Database
```

In an ideal world this reference file could then be included as supplementary information in any
life science publication that relies on genomic information so that
reproducibility of experiments and analyses becomes achievable.


### Genome retrieval of hundreds of genomes using only one command

Download all mammalian vertebrate genomes from `NCBI RefSeq` via:

```r
# download all vertebrate genomes
meta.retrieval(kingdom = "vertebrate_mammalian", db = "refseq", type = "genome")
```
All geneomes are stored in the folder named according to the kingdom.
In this case `vertebrate_mammalian`. Alternatively, users can specify
the `out.folder` argument to define a custom output folder path.

### Platforms

> Find `biomartr` also at [OmicTools](https://omictools.com/biomartr-tool).

### Frequently Asked Questions (FAQs)

Please find [all FAQs here](FAQs.md).

### Discussions and Bug Reports

I would be very happy to learn more about potential improvements of the concepts and functions
provided in this package.

Furthermore, in case you find some bugs or need additional (more flexible) functionality of parts
of this package, please let me know:

[twitter: HajkDrost](https://twitter.com/hajkdrost) or  [email](hgd23@cam.ac.uk)

For Bug Reports: Please send me an [issue](https://github.com/HajkD/biomartr/issues).


## Tutorials

Getting Started with `biomartr`:

- [NCBI Database Retrieval](https://docs.ropensci.org/biomartr/articles/Database_Retrieval.html)
- [Genomic Sequence Retrieval](https://docs.ropensci.org/biomartr/articles/Sequence_Retrieval.html)
- [Meta-Genome Retrieval](https://docs.ropensci.org/biomartr/articles/MetaGenome_Retrieval.html)
- [Functional Annotation](https://docs.ropensci.org/biomartr/articles/Functional_Annotation.html)
- [BioMart Examples](https://docs.ropensci.org/biomartr/articles/BioMart_Examples.html)


Users can also read the tutorials within ([RStudio](http://www.rstudio.com/)) :

```r
# source the biomartr package
library(biomartr)

# look for all tutorials (vignettes) available in the biomartr package
# this will open your web browser
browseVignettes("biomartr")
```

## NEWS
The current status of the package as well as a detailed history of the functionality of each version of `biomartr` can be found in the [NEWS](https://docs.ropensci.org/biomartr/news/index.html) section.


## Install Developer Version
Some bug fixes or new functionality will not be available on CRAN yet, but in the developer version here on GitHub. To download and install the most recent version of `biomartr` run:

```r
# install the current version of biomartr on your system
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ropensci/biomartr")
```

## Genomic Data Retrieval

#### Meta-Genome Retrieval

* `meta.retrieval()` : Perform Meta-Genome Retieval from NCBI of species belonging to the same kingdom of life or to the same taxonomic subgroup
* `meta.retrieval.all()` : Perform Meta-Genome Retieval from NCBI of the entire kingdom of life
* `getMetaGenomes()` : Retrieve metagenomes from NCBI Genbank
* `getMetaGenomeAnnotations()` : Retrieve annotation *.gff files for metagenomes from NCBI Genbank
* `listMetaGenomes()` : List available metagenomes on NCBI Genbank
* `getMetaGenomeSummary()` : Helper function to retrieve the assembly_summary.txt file from NCBI genbank metagenomes
* `clean.retrieval()`: Format meta.retrieval output

#### Genome Retrieval

* `listGenomes()` : List all genomes available on NCBI and ENSEMBL servers
* `listKingdoms()` : list the number of available species per kingdom of life on NCBI and ENSEMBL servers
* `listGroups()` : list the number of available species per group on NCBI and ENSEMBL servers
* `getKingdoms()` : Retrieve available kingdoms of life
* `getGroups()` : Retrieve available groups for a kingdom of life
* `is.genome.available()` : Check Genome Availability  NCBI and ENSEMBL servers
* `getCollection()` : Retrieve a Collection: Genome, Proteome, CDS, RNA, GFF, Repeat Masker, AssemblyStats
* `getGenome()` : Download a specific genome stored on NCBI and ENSEMBL servers
* `getGenomeSet()` : Genome Retrieval of multiple species
* `getProteome()` : Download a specific proteome stored on NCBI and ENSEMBL servers
* `getProteomeSet()` : Proteome Retrieval of multiple species
* `getCDS()` : Download a specific CDS file (genome) stored on NCBI and ENSEMBL servers
* `getCDSSet()` : CDS Retrieval of multiple species
* `getRNA()` : Download a specific RNA file stored on NCBI and ENSEMBL servers
* `getRNASet()` : RNA Retrieval of multiple species
* `getGFF()` : Genome Annotation Retrieval from NCBI (`*.gff`) and ENSEMBL (`*.gff3`) servers
* `getGTF()` : Genome Annotation Retrieval (`*.gtf`) from ENSEMBL servers
* `getRepeatMasker() :` Repeat Masker TE Annotation Retrieval
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
* `read_rm()` : Import Repeat Masker output file
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


