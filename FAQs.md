# FAQs

- [Why do I get FTP errors when I download hundreds or thousands of genomes (e.g. bacteria or viruses) with biomartr?](https://github.com/HajkD/biomartr/issues/12)
    - > Short Answer: NCBI and Ensembl limit the server access for individual connections. Hence, when hundreds of retrieval queries are sent to download hundreds of genomes, it may happen that some of the queries get cancelled after a certain count. In that case, simply re-run the corresponding `meta.retrieval` function and it will pick up where it left off. 
- [How is the `biomartr` package different from the `BiomaRt` package that I have been using so far?](https://github.com/HajkD/biomartr/issues/11)
    - > Short Answer: `biomartr` is designed for the retrieval of big datasets such as multiple genomes, proteomes, annotation, etc whereas `biomaRt` is designed for the retrieval of small data such as sequences of individual genes 
- [How can I retrieve GO terms for human genes using ensembl gene IDs?](https://github.com/HajkD/biomartr/issues/5)
- [How can I download coding sequences or protein sequences for all sequenced species?](https://www.biostars.org/p/9202/#235962)
- [Where can I Download the human reference genome in fasta format?](https://www.biostars.org/p/1796/#236039)
