context("Test: getGenome()")

test_that("The getGenome() interface works properly for NCBI RefSeq (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <- read_genome(
        getGenome(
            db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_genome(
        getGenome(
            db       = "refseq",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    expect_s4_class(out1, class = "DNAStringSet")
    expect_false(any(out1 != out2))
})


test_that("The getGenome() interface works properly for NCBI RefSeq using taxid..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <- read_genome(
        getGenome(
            db       = "refseq",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
})

test_that("The getGenome() interface works properly for NCBI RefSeq using assembly id..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <- read_genome(
        getGenome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
})



test_that("The getGenome() interface works properly for NCBI Genbank (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
    expect_false(any(out1 != out2))
})

test_that("The getGenome() interface works properly for NCBI Genbank using taxid (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
    expect_false(any(out1 != out2))
})

test_that("The getGenome() interface works properly for NCBI Genbank using accession ids (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_genome(
        getGenome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
    expect_false(any(out1 != out2))
})

test_that("The getGenome() interface works properly for ENSEMBL (including repeating function call)..",{

        skip_on_cran()
        skip_on_travis()
    # test proper download from ENSEMBL
        out1 <- read_genome(
            getGenome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

        out2 <- read_genome(
            getGenome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )
        expect_s4_class(out1, class = "DNAStringSet")
        expect_false(any(out1 != out2))
})

test_that("The getGenome() interface works properly for ENSEMBL using taxid (including repeating function call)..",{

    skip_on_cran()
    skip_on_travis()
    # test proper download from ENSEMBL
    out1 <- read_genome(
        getGenome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_genome(
        getGenome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
    expect_false(any(out1 != out2))
})


test_that("The getGenome() interface works properly for ENSEMBL using accession id (including repeating function call)..",{

    skip_on_cran()
    skip_on_travis()
    # test proper download from ENSEMBL
    out1 <- read_genome(
        getGenome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_genome(
        getGenome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_s4_class(out1, class = "DNAStringSet")
    expect_false(any(out1 != out2))
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

    out1 <- getGenome(
        db       = "ensembl",
        organism = "Saccharomycesi",
        path     = tempdir(), mute_citation = TRUE
    )
    expect_type(out1, "logical")
    expect_false(out1 == TRUE)
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
