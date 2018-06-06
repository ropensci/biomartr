context("Test: getProteome()")

test_that("The getProteome() interface to NCBI RefSeq works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    Proteome <-
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
    # test proper use of internal referece files when command is repeated
    Proteome <-
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
})


test_that("The getProteome() interface to NCBI RefSeq works properly using taxid (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "559292",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
    # test proper use of internal referece files when command is repeated
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "559292",
                path     = tempdir()
            ),
            format = "fasta"
        )
})


test_that("The getProteome() interface to NCBI RefSeq works properly using accession id (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    read_proteome(
        getProteome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    read_proteome(
        getProteome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
})

test_that("The getProteome() interface to NCBI Genbank works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis() 
    # test proper download from genbank
        read_proteome(
            getProteome(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
    # test proper use of internal referece files when command is repeated
        read_proteome(
            getProteome(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
})


test_that("The getProteome() interface to NCBI Genbank works properly using taxid (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    read_proteome(
        getProteome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    read_proteome(
        getProteome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir()
        ),
        format = "fasta"
    )
})


test_that("The getProteome() interface to NCBI Genbank works properly using accession (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    read_proteome(
        getProteome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    # test proper use of internal referece files when command is repeated
    read_proteome(
        getProteome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
})

test_that("The getProteome() interface to Ensembl works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
        read_proteome(
            getProteome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
        read_proteome(
            getProteome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
})

test_that("The getProteome() interface to Ensembl works properly using taxid (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir()
        ),
        format = "fasta"
    )
})


test_that("The getProteome() interface to Ensembl works properly using accession ids (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
    
    read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir()
        ),
        format = "fasta"
    )
})


test_that("The getProteome() interface to EnsemblGenomes works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()    
        read_proteome(
            getProteome(
                db       = "ensemblgenomes",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
        
        read_proteome(
            getProteome(
                db       = "ensemblgenomes",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir()
            ),
            format = "fasta"
        )
    
})

test_that("The getProteome() interface to EnsemblGenomes works properly using taxids..", {
    skip_on_cran()
    skip_on_travis()    
    expect_error(read_proteome(
        getProteome(
            db       = "ensemblgenomes",
            organism = "4932",
            path     = tempdir()
        ),
        format = "fasta"
    ))
    
})

test_that("The getProteome() interface to EnsemblGenomes works properly using accession ids..", {
    skip_on_cran()
    skip_on_travis()    
    expect_error(read_proteome(
        getProteome(
            db       = "ensemblgenomes",
            organism = "GCA_000976725.1",
            path     = tempdir()
        ),
        format = "fasta"
    ))
    
})

test_that("The getProteome() error messages work properly..", {
    skip_on_cran()
    skip_on_travis()     
    expect_warning(getProteome(
        db       = "ensembl",
        organism = "Saccharomyces cerevisi",
        path     = tempdir()
    ))
})
