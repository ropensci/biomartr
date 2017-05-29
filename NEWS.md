biomartr 0.6.0
===========

### New Functions


biomartr 0.5.1
===========

### Bug fixes

- fixing a bug in `exists.ftp.file()` and `getENSEMBLGENOMES.Seq()` that caused bacterial genome, proteome, etc retrieval to fail due to the wrong construction of a query ftp request https://github.com/HajkD/biomartr/issues/7
(Many thanks to @dbsseven)

- fix a major bug in which organisms having no representative genome would generate NULL paths that subsequently crashed the `meta.retrieval()` function when it tried to print out the result paths.

### New Functions

- new function `getRepeatMasker()` for retrieval of Repeat Masker output files  

- new function `getGTF()` for genome annotation retrieval from `ensembl` and `ensemblgenomes` in `gtf` format (Thanks for suggesting it Ge Tan)

- new function `getRNA()` to perform RNA Sequence Retrieval from NCBI and ENSEMBL databases (Thanks for suggesting it @carlo-berg)

- new function `read_rna()` for importing Repeat Masker output files downloaded with `getRepeatMasker()`

- new function `read_rm()` for importing RNA downloaded with `getRNA()` as Biostrings or data.table object

- new helper function `custom_download()` that aims to make the download process more robust and stable
-> In detail, the download process is now adapting to the operating system, e.g. using either `curl` (macOS), `wget` (Linux), or `wininet` (Windows)




### Function changes

- function name `listDatabases()` has been renamed `listNCBIDatabases()`. In `biomartr` version 0.6.0 the function name `listDatabases()` will be depreciated

- `meta.retieval()` and `meta.retieval.all()` now allow the bulk retrieval of GTF files for `type = 'ensembl'` and `type = 'esnemblgenomes'` via `type = "gtf"`. See `getGTF()` for more details.

- `meta.retieval()` and `meta.retieval.all()` now allow the bulk retrieval of RNA files via `type = "rna"`. See `getRNA()` for more details.

- `meta.retieval()` and `meta.retieval.all()` now allow the bulk retrieval of Repeat Masker output files via `type = "rm"`. See `getRepeatMasker()` for more details.

- all `get*()` retrieval functions now skip the download of a particular file if it already exists in the specified file path

- `download.database()` and `download.database.all()` now internally perform md5 check sum checks to make sure that the file download was successful

- `download.database()` and `download.database.all()` now return the file paths of the downloaded file so that it is easier to use these
functions when constructing pipelines, e.g. `download.database() %>% ...` or `download.database.all() %>% ...`.

- `meta.retrieval()` and `meta.retrieval.all()` now return the file paths of the downloaded file so that it is easier to use these
functions when constructing pipelines, e.g. `meta.retrieval() %>% ...` or `meta.retrieval() %>% ...`.

- `getGenome()`, `getProteome()`, `getCDS()`, `getRNA()`, `getGFF()`, and `getAssemblyStats()` now internally perform md5 checksum tests
to make sure that files are retrieved intact.


biomartr 0.4.0
===========

### Bug fixes

- fixing a major bug https://github.com/HajkD/biomartr/issues/6 that caused that in all `get*()` (genome, proteome, gff, etc.) and `meta.retrieval*()` functions
 the meta retrieval process errored and terminated whenever NCBI or ENSEMBL didn't
store all types of sequences for a particular organism: genome, proteome, cds, etc. This has been fixed now and function calls
such as `meta.retrieval(kingdom = "bacteria", db = "genbank", type = "proteome")` should work properly now (Thanks to @ARamesh123 for making me aware if this bug). Hence, this bug affected all attempts to download all proteome sequences e.g. for bacteria and viruses, because NCBI does not store genome AND proteome information for all bacterial or viral species. 


### New Functions

- new function `getAssemblyStats()` allows users to retrieve the genome assembly stats file from NCBI RefSeq or Genbank, e.g. ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/001/405/GCF_000001405.36_GRCh38.p10/GCF_000001405.36_GRCh38.p10_assembly_stats.txt

- new function `read_assemblystats()` allows to import the genome assembly stats file from NCBI RefSeq or Genbank that was retrieved
using the `getAssemblyStats()` function

### Function changes

- `meta.retrieval()` and `meta.retrieval.all()` can now also download genome assembly stats for all selected species

- `meta.retrieval()` receives a new argument `group` that allows users to retrieve species belonging to a subgroup instead of the entire kingdom.
Available groups can be retrieved with `getGroups()`.

- functions `getSubgroups()` and `listSubgroups()` have been removed and their initial functionality
has been merged and integrated into `getGroups()` and `listGroups()`

- `listGroups()` receives a new argument `details` that allows users to retrieve the organism names that belong to the corresponding subgroups

- `getGroups()` is now based on `listGroups()`

- internal function `getGENOMESREPORT()` is now exported and available to the user

- all `organism*()` functions now also support Ensembl Plants, Ensembl Metazoa, Ensembl Protist, and Ensembl Fungi (Thanks for pointing out [Alex Gabel](https://github.com/AlexGa))

- `getMarts()` and `getDatasets()` now also support Ensembl Plants, Ensembl Metazoa, Ensembl Protist, and Ensembl Fungi (Thanks for pointing out [Alex Gabel](https://github.com/AlexGa))


### Vignette updates

- Vignette `Meta-Genome Retrieval` has more examples how to download genomes of species that belong to the same subgroup


biomartr 0.3.0
===========

### Bug fixes

- Fixing a bug https://github.com/HajkD/biomartr/issues/2 based on the [readr package](https://github.com/tidyverse/readr) that affected the `getSummaryFile()`, `getKingdomAssemblySummary()`, `getMetaGenomeSummary()`,
`getENSEMBL.Seq()` and `getENSEMBLGENOMES.Seq()` functions causing quoted lines in the `assembly_summary.txt` to be omitted when reading these files. This artefact caused that e.g. instead of information of 80,000 Bacteria genomes only 40,000 (which non-quotations) were read (Thanks to [Xin Wu](https://github.com/alartin)).


biomartr 0.2.1
===========

In this version of `biomartr` the `organism*()` functions were adapted to the new [ENSEMBL 87 release](http://www.ensembl.info/blog/2016/12/08/ensembl-87-has-been-released/)
in which organism name specification in the Biomart description column [was changed](https://github.com/HajkD/biomartr/issues/1)
from a scientific name convention to a mix of common name and scientific name convention.

- all `organism*()` functions have been adapted to the new ENSEMBL 87 release organism name notation that is used in the Biomart description

- fixing error handling bug that caused commands such as `download.database(db = "nr.27.tar.gz")` to not execute properly

biomartr 0.2.0
===========

In this version, `biomartr` was extended to now retrieve genome, proteome, CDS, GFF and meta-genome data
also from [ENSEMBL](http://www.ensembl.org/index.html) and [ENSEMLGENOMES](http://ensemblgenomes.org/).
Furthermore, all NCBI retrieval functions were updated to the new server folder structure standards of NCBI.


### New Functions

- new meta-retrieval function `meta.retrieval.all()` allows users to download all individual genomes of all kingdoms of life with one command

- new metagenome retrieval function `getMetaGenomes()` allows users to retrieve metagenome projects from NCBI Genbank

- new metagenome retrieval function `getMetaGenomeAnnotations()` allows users to retrieve annotation files for genomes belonging to a metagenome project stored at NCBI Genbank

- new retrieval function `getGFF()` allows users to retrieve annotation (*.gff) files for specific genomes from NCBI and ENSEMBL databases

- new import function `read_gff()` allowing users to import GFF files downloaded with `getGFF()`

- new internal functions to check for availability of ENSEMBL or ENSEMBLGENOMES databases

- new database retrieval function `download.database.all()` allows users to download entire NCBI databases with one command

- new function `listMetaGenomes()` allowing users to list available metagenomes on NCBI Genbank

- new external helper function `getSummaryFile()` to retrieve the assembly_summary.txt file from NCBI

- new external helper function `getKingdomAssemblySummary()` to retrieve the assembly_summary.txt files from NCBI for all kingdoms and combine them
into one big data.frame

- new function `listKingdoms()` allows users to list the number of available species per kingdom of life

- new function `listGroups()` allows users to list the number of available species per group

- new function `listSubgroups()` allows users to list the number of available species per subgroup

- new function `getGroups()` allows users to retrieve available groups for a kingdom of life

- new function `getSubgroups()` allows users to retrieve available subgroups for a kingdom of life

- new external helper function `getMetaGenomeSummary()` to retrieve the assembly_summary.txt files from NCBI genbank metagenomes

- new internal helper function `getENSEMBL.Seq()` acting as main interface function to communicate with the ENSEMBL database API for sequence retrieval

- new internal helper function `getENSEMBLGENOMES.Seq()` acting as main interface function to communicate with the ENSEMBL database API for sequence retrieval

- new internal helper function `getENSEMBL.Annotation()` acting as main interface function to communicate with the ENSEMBL database API for GFF retrieval

- new internal helper function `getENSEMBLGENOMES.Annotation()` acting as main interface function to communicate with the ENSEMBL database API for GFF retrieval

- new internal helper function `get.ensemblgenome.info()` to retrieve general organism information from ENSEMBLGENOMES 

- new internal helper function `get.ensembl.info()` to retrieve general organism information from ENSEMBL 

- new internal helper function `getGENOMEREPORT()` to retrieve the genome reports file from ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/overview.txt

- new internal helper function `connected.to.internet()` enabling internet connection check

### Function changes

- functions `getGenome()`, `getProteome()`, and `getCDS()` now can also in addition to NCBI retrieve genomes, proteomes or CDS from  [ENSEMBL](http://www.ensembl.org/index.html) and [ENSEMLGENOMES](http://ensemblgenomes.org/)

- the functions `getGenome()`, `getProteome()`, and `getCDS()` were completely re-written and now use the assembly_summary.txt files
provided by NCBI to retrieve the download path to the corresponding genome. Furthermore, these functions now lost the `kingdom` argument.
Users now only need to specify the organism name and not the kingdom anymore. Furthermore, all `get*` functions now
return the path to the downloaded genome so that this path can be used as input to all `read_*` functions.

- `download_databases()` has been renamed to `download.databases()` to be more consistent with other function notation

- the argument `db_format` was removed from `listDatabases()` and `download.database()` because it was misleading

- the command `listDatabases("all")` now returns all available NCBI databases that can be retrieved with `download.database()`

- `download.database()` now internally checks if input database specified by the user is actually available on NCBI servers

- the documentary file generated by `getGenome()`, `getProteome()`, and `getCDS()` is now extended to store more details about the downloaded genome

- argument `database` in `is.genome.available()` and `listGenomes()` has been renamed to `db` to be consistent with all other sequence retrieval functions

- `is.genome.available()` now also checks availability of organisms in ENSEMBL. See `db = "ensembl"`

- the argument `db_name` in `listDatabases()` has been renamed `db` to be more consistent with the notation in other functions

- the argument `name` in `download.database()` has been renamed `db` to be more consistent with the notation in other functions

- `getKingdoms()` now retrieves also kingdom information for ENSEMBL and ENSEMBLGENOMES

- `getKingdoms()` received new argument `db` to specify from which database (e.g. `refseq`, `genbank`, `ensembl` or `ensemblgenomes`) kingdom information shall be retrieved

- `getKingdoms(db = "refseq")` received one more member: `"viral"`, allowing the genome retrieval of all viruses

- argument `out.folder` in `meta.retrieval()` has been renamed to `path` to be more consistent with other retrieval functions

- all `read_*` functions now received a new argument `obj.type` allowing users to choose between storing input genomes as Biostrings object or data.table object

- all `read_*` functions now have `format = "fasta"` as default

- the `kingdom` argument in the `listGenomes()` function was renamed to `type`, now allowing users to specify not only specify kingdoms,
but also groups and subgroups. Use: `listGenomes(type = "kingdom")` or `listGenomes(type = "group")` or `listGenomes(type = "subgroup")`

- the `listGenomes()` function receives a new argument `subset` to specify a subset of the selected `type` argument. E.g. `subset = "Eukaryota"` when specifying
`type = "kingdom"`


### Vignette updates

- new Vignette `Meta-Genome Retrieval`
- Update examples and extend `Introduction` Vignette 
- Update examples and extend `Database Retrieval` Vignette 
- Update examples and extend `Sequence Retrieval` Vignette
- Update examples and extend `Functional Annotation` Vignette


biomartr 0.1.0
===========

- fixing a parsing error of the file `ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_mammalian/assembly_summary.txt`
The problem was that comment lines were introduced and columns couldn't be parsed correctly anymore. This caused that genomes, proteomes, and CDS files could not be downloaded properly. This has been fixed now.

- genomes, proteome, and CDS as well as meta-genomes can now be retrieved
from RefSeq and Genbank (not only RefSeq); only `getCDS()` does not have genebank access,
becasue genbank does not provide CDS sequences

- adding new function `meta.retrieval()` to mass retrieve genomes for entire kingdoms of life 

- fixed a major bug in `organismBM()` causing the function to fail. The failure of
this function affected all downstream `organism*()` functions. Bug is now fixed and everything
works properly

- updated Vignettes

biomartr 0.0.3
===========

- updating unit tests for new API

- fixing API problems that caused all BioMart related functions to fail

- fixing retrieval problems in `getCDS()`, `getProteome()`, and `getGenome()`

- the `listDatabases()` function now has a new option `db_name = "all"` allowing users to list all available databases stored on NCBI 

### Vignettes
- adding new vignette: Database Retrieval
- update the vignettes: Phylotranscriptomics, Sequence Retrieval, and Functional Annotation


biomartr 0.0.2
===========

### Vignettes
- adding vignettes: Introduction, Functional Annotation, Phylotranscriptomics, and Sequence Retrieval

biomartr 0.0.1
===========

Release Version
