biomartr
========

[![rpackages.io rank](https://www.rpackages.io/badge/biomartr.svg)](https://www.rpackages.io/package/biomartr)
[![](https://badges.ropensci.org/93_status.svg)](https://github.com/ropensci/onboarding/issues/93)

## Genomic Data Retrieval with R

### Motivation:

This package is born out of my own frustration to automate the genomic data retrieval process to create computationally reproducible scripts for large-scale genomics studies. Since I couldn't find easy-to-use and fully reproducible software libraries I sat down and tried to implement a framework that would enable anyone to automate and standardize the genomic data retrieval process. I hope that this package is useful to others as well and that it helps to promote reproducible research in genomics studies.

I happily welcome anyone who wishes to contribute to this project :) Just drop me an email.

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
- [ENSEMBLGENOMES](http://ensemblgenomes.org)
- [UniProt](http://www.uniprot.org)

Furthermore, an interface to the [Ensembl Biomart](www.ensembl.org/biomart) database allows users to retrieve functional annotation for genomic loci using a novel and organism centric search strategy. In addition, users can [download entire databases](https://github.com/HajkD/biomartr/blob/master/vignettes/Database_Retrieval.Rmd) such as `NCBI RefSeq`, `NCBI nr`, `NCBI nt`, `NCBI Genbank`, etc. as well as `ENSEMBL` and `ENSEMBLGENOMES` with only one command.

### Citation

**I would be very greatful if you could cite the following paper in case `biomartr` was useful for your own research. I plan on vastly extending 
the biomartr functionality and usability in the next years. Many thanks in advance :)**

> Drost HG, Paszkowski J. __Biomartr: genomic data retrieval with R__. *Bioinformatics* (2017) 33(8): 1216-1217. [doi:10.1093/bioinformatics/btw821](https://academic.oup.com/bioinformatics/article/doi/10.1093/bioinformatics/btw821/2931816/Biomartr-genomic-data-retrieval-with-R).


### Feedback
>__I truly value your opinion and improvement suggestions. Hence, I would be extremely grateful if you could take this 1 minute and 3 question survey (https://goo.gl/forms/Qaoxxjb1EnNSLpM02) so that I can learn how to improve `biomartr` in the best possible way. Many many thanks in advance.__


## Installation

```r
# install biomartr 0.7.0
source("http://bioconductor.org/biocLite.R")
biocLite('biomartr')
```

### Example

```r
# download the genome of Homo sapiens from refseq
# and store the corresponding genome file in '_ncbi_downloads/genomes'
HS.genome.refseq <- getGenome( db       = "refseq", organism = "Homo sapiens")
```

Subsequently, users can use the read_genome() function to import the genome into the R session. Users can choose to work with the genome sequence in R either as Biostrings object (`obj.type = "Biostrings"`) or data.table object (`obj.type = "data.table"`) by specifying the obj.type argument of the read_genome() function.

```r
# import downloaded genome as Biostrings object
Human_Genome <- read_genome(file     = HS.genome.refseq)
```

```
# look at the Biostrings object
Human_Genome
  A DNAStringSet instance of length 551
          width seq                                                names               
  [1] 248956422 NNNNNNNNNNNNNNNNNNNNNNNN...NNNNNNNNNNNNNNNNNNNNNNN NC_000001.11 Homo...
  [2]    175055 GAATTCAGCTGAGAAGAACAGGCA...TGTTTGTCAGTCACATAGAATTC NT_187361.1 Homo ...
  [3]     32032 AGGGGTCTGCTTAGAGAGGGTCTC...TGACTTACGTTGACGTGGAATTC NT_187362.1 Homo ...
  [4]    127682 GATCGAGACTATCCTGGCTAACAC...ATTGTCAATTGGGACCTTTGATC NT_187363.1 Homo ...
  [5]     66860 GAATTCATTCGATGACGATTCCAT...AAAAAACTCTCAGCCACGAATTC NT_187364.1 Homo ...
  ...       ... ...
[547]    170148 TTTCTTTCTTTTTTTTTTTTTTGT...GTCACAGGACTCATGGGGAATTC NT_187685.1 Homo ...
[548]    215732 TGTGGTGAGGACCCTTAAGATCTA...GTCACAGGACTCATGGGGAATTC NT_187686.1 Homo ...
[549]    170537 TCTACTCTCCCATGCTTGCCTCGG...GTCACAGGACTCATGGGGAATTC NT_187687.1 Homo ...
[550]    177381 GATCTATCTGTATCTCCACAGGTG...GTCACAGGACTCATGGGGAATTC NT_113949.2 Homo ...
[551]     16569 GATCACAGGTCTATCACCCTATTA...CCCTTAAATAAGACATCACGATG NC_012920.1 Homo ...
```

Internally, a text file named `doc_Homo_sapiens_db_refseq.txt` is generated. The information stored in this log file is structured as follows:

```
File Name: Homo_sapiens_genomic_refseq.fna.gz
Organism Name: Homo_sapiens
Database: NCBI refseq
URL: ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/001/405/
GCF_000001405.35_GRCh38.p9/GCF_000001405.35_GRCh38.p9_genomic.fna.gz
Download_Date: Sat Oct 22 12:41:07 2016
refseq_category: reference genome
assembly_accession: GCF_000001405.35
bioproject: PRJNA168
biosample: NA
taxid: 9606
infraspecific_name: NA
version_status: latest
release_type: Patch
genome_rep: Full
seq_rel_date: 2016-09-26
submitter: Genome Reference Consortium
```

In an ideal world this reference file could then be included as supplementary information in any
life science publication that relies on genomic information so that
reproducibility of experiments and analyses becomes achievable.


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

- [NCBI Database Retrieval](https://ropensci.github.io/biomartr/articles/Database_Retrieval.html)
- [Genomic Sequence Retrieval](https://ropensci.github.io/biomartr/articles/Sequence_Retrieval.html)
- [Meta-Genome Retrieval](https://ropensci.github.io/biomartr/articles/MetaGenome_Retrieval.html)
- [Functional Annotation](https://ropensci.github.io/biomartr/articles/Functional_Annotation.html)
- [BioMart Examples](https://ropensci.github.io/biomartr/articles/BioMart_Examples.html)


Users can also read the tutorials within ([RStudio](http://www.rstudio.com/)) :

```r
# source the biomartr package
library(biomartr)

# look for all tutorials (vignettes) available in the biomartr package
# this will open your web browser
browseVignettes("biomartr")
```

## NEWS
The current status of the package as well as a detailed history of the functionality of each version of `biomartr` can be found in the [NEWS](https://ropensci.github.io/biomartr/news/index.html) section.


## Install Developer Version
Some bug fixes or new functionality will not be available on CRAN yet, but in the developer version here on GitHub. To download and install the most recent version of `biomartr` run:

```r
# install the current version of biomartr on your system
source("http://bioconductor.org/biocLite.R")
biocLite("ropensci/biomartr")
```

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
* `getCollection()` : Retrieve a Collection: Genome, Proteome, CDS, RNA, GFF, Repeat Masker, AssemblyStats
* `getGenome()` : Download a specific genome stored on NCBI and ENSEMBL servers
* `getProteome()` : Download a specific proteome stored on NCBI and ENSEMBL servers
* `getCDS()` : Download a specific CDS file (genome) stored on NCBI and ENSEMBL servers
* `getRNA()` : Download a specific RNA file stored on NCBI and ENSEMBL servers
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


