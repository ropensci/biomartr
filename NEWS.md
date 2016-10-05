biomartr 0.2.0
===========

## New Functions

- new helper function `getSummaryFile()` to retrieve the assembly_summary.txt file from NCBI

## Function changes

- the argument `db_format` was removed from `listDatabases()` and `download_database()` because it was misleading

- the command `listDatabases("all")` now returns all available NCBI databases that can be retrieved with `download_database()`

- `download_database()` now internally checks if input database specified by the user is actually available on NCBI servers


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
