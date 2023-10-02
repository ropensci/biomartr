context("Test: getProteome()")

test_that("The getProteome() interface to NCBI RefSeq works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    out1 <-
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

    # test proper use of internal referece files when command is repeated
    out2 <-
        read_proteome(
            getProteome(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )
    expect_false(length(out1) != length(out2) | length(out1) == 0)
})


test_that("The getProteome() interface to NCBI RefSeq works properly using taxid (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    out1 <-   read_proteome(
            getProteome(
                db       = "refseq",
                organism = "559292",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

    # test proper use of internal referece files when command is repeated
    out2 <-  read_proteome(
            getProteome(
                db       = "refseq",
                organism = "559292",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )
        expect_false(length(out1) != length(out2) | length(out1) == 0)
})


test_that("The getProteome() interface to NCBI RefSeq works properly using accession id (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    out1 <- read_proteome(
        getProteome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_proteome(
        getProteome(
            db       = "refseq",
            organism = "GCF_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_false(length(out1) != length(out2) | length(out1) == 0)
})

test_that("The getProteome() interface to NCBI Genbank works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from genbank
    out1 <-   read_proteome(
            getProteome(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

    # test proper use of internal referece files when command is repeated
    out2 <-   read_proteome(
            getProteome(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )
        expect_false(length(out1) != length(out2) | length(out1) == 0)
})


test_that("The getProteome() interface to NCBI Genbank works properly using taxid (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    out1 <- read_proteome(
        getProteome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_proteome(
        getProteome(
            db       = "genbank",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_false(length(out1) != length(out2) | length(out1) == 0)
})


test_that("The getProteome() interface to NCBI Genbank works properly using accession (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download
    out1 <- read_proteome(
        getProteome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    # test proper use of internal referece files when command is repeated
    out2 <- read_proteome(
        getProteome(
            db       = "genbank",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_false(length(out1) != length(out2) | length(out1) == 0)
})

test_that("The getProteome() interface to Ensembl works properly (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    out1 <- read_proteome(
            getProteome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

    out2 <-  read_proteome(
            getProteome(
                db       = "ensembl",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )
        expect_false(length(out1) != length(out2) | length(out1) == 0)
})

test_that("The getProteome() interface to Ensembl works properly using taxid (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    out1 <- read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_false(length(out1) != length(out2) | length(out1) == 0)
})


test_that("The getProteome() interface to Ensembl works properly using accession ids (including repeating command)..", {
    skip_on_cran()
    skip_on_travis()
    out1 <- read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_proteome(
        getProteome(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )
    expect_false(length(out1) != length(out2) | length(out1) == 0)
})

test_that("The getProteome() interface to Ensembl works properly (For organisms with collections)..", {
  skip_on_cran()
  skip_on_travis()

  out <- getProteome(
    db       = "ensembl",
    organism = "Escherichia coli",
    path     = tempdir(), mute_citation = TRUE
  )

  expect_false(is.logical(out))
})

test_that("The getProteome() interface to Uniprot works properly ..", {
  skip_on_cran()
  skip_on_travis()

  out1 <- getProteome(
    db       = "uniprot",
    organism = "Saccharomyces cerevisiae",
    path     = tempdir(), mute_citation = TRUE
  )

  out2 <- getProteome(
    db       = "uniprot",
    organism = "Homo sapiens",
    path     = tempdir(), mute_citation = TRUE
  )

  expect_false(is.logical(c(out1, out2)))
  expect_false(anyNA(c(out1, out2)))
  expect_false(identical(out1, out2))
})




