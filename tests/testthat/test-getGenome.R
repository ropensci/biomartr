context("Test: getGenome()")

test_that("The getGenome() interface works properly for NCBI RefSeq (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    expect_output(read_genome(
        getGenome(
            db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))

    # test proper use of internal referece files when command is repeated
    expect_output(read_genome(
        getGenome(
            db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})


test_that("The getGenome() interface works properly for NCBI RefSeq using taxid..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    expect_output(read_genome(
        getGenome(
            db       = "refseq",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})

test_that("The getGenome() interface works properly for NCBI RefSeq using assembly id..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    expect_output(read_genome(
        getGenome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})



test_that("The getGenome() interface works properly for NCBI Genbank (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    expect_output(read_genome(
        getGenome(
            db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))

    # test proper use of internal referece files when command is repeated
    expect_output(read_genome(
        getGenome(
            db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})

test_that("The getGenome() interface works properly for NCBI Genbank using taxid (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    expect_output(read_genome(
        getGenome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))

    # test proper use of internal referece files when command is repeated
    expect_output(read_genome(
        getGenome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})

test_that("The getGenome() interface works properly for NCBI Genbank using accession ids (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    expect_output(read_genome(
        getGenome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))

    # test proper use of internal referece files when command is repeated
    expect_output(read_genome(
        getGenome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})

test_that("The getGenome() interface works properly for ENSEMBL (including repeating function call)..",{

        skip_on_cran()
        skip_on_travis()
    # test proper download from ENSEMBL
        expect_output(read_genome(
            getGenome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        ))

        expect_output(read_genome(
            getGenome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        ))
})

test_that("The getGenome() interface works properly for ENSEMBL using taxid (including repeating function call)..",{

    skip_on_cran()
    skip_on_travis()
    # test proper download from ENSEMBL
    expect_output(read_genome(
        getGenome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))

    expect_output(read_genome(
        getGenome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})


test_that("The getGenome() interface works properly for ENSEMBL using accession id (including repeating function call)..",{

    skip_on_cran()
    skip_on_travis()
    # test proper download from ENSEMBL
    expect_output(read_genome(
        getGenome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))

    expect_output(read_genome(
        getGenome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    ))
})

test_that("The getGenome() interface works properly for ENSEMBL with collections",{

  skip_on_cran()
  skip_on_travis()
  # test proper download from ENSEMBL
  # Bacteria
  out1 <- getGenome(db       = "ensembl",
            organism = "Escherichia coli",
            path = tempdir(), mute_citation = TRUE)
  # Fungi
  out2 <- getGenome(db       = "ensembl",
            organism = "Acremonium chrysogenum",
            path = tempdir(), mute_citation = TRUE)
  # Protists
  getGenome(db       = "ensembl",
            organism = "Babesia bigemina",
            path = tempdir(), mute_citation = TRUE)
  expect_false(out1 == out2)
})

test_that("The getGenome() error messages work properly for ENSEMBL..", {
    skip_on_cran()
    skip_on_travis()

    expect_output(getGenome(
        db       = "ensembl",
        organism = "Saccharomyces cerevisi",
        path     = tempdir(), mute_citation = TRUE
    ))
})

test_that("The getGenome() error messages work properly for NCBI RefSeq", {
    skip_on_cran()
    skip_on_travis()

    expect_equal(getGenome(
        db       = "refseq",
        organism = "Saccharomycesi",
        path     = tempdir(), mute_citation = TRUE
    ), "Not available")
})
