context("Test: getRNA()")

test_that(
    "The getRNA() interface to NCBI RefSeq works properly (including when command is repeated)..",
    {
        skip_on_cran()
        skip_on_travis()
        # test proper download
        out1 <- read_rna(
            getRNA(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

        # test proper use of internal referece files when command is repeated
        out2 <- read_rna(
            getRNA(
                db       = "refseq",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )
      expect_equal(out1, out2)
    }
)


test_that(
    "The getRNA() interface to NCBI RefSeq works properly uding taxids (including when command is repeated)..",
    {
        skip_on_cran()
        skip_on_travis()
        # test proper download
        out1 <- read_rna(getRNA(
            db       = "refseq",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta")

        out2 <- read_rna(getRNA(
            db       = "refseq",
            organism = "559292",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta")

        expect_equal(out1, out2)
    }
)

test_that("The getRNA() interface to NCBI RefSeq works properly using assembly ids ..",
          {
              skip_on_cran()
              skip_on_travis()
              # test proper download
              out1 <- read_rna(getRNA(
                  db       = "refseq",
                  organism = "GCF_000146045.2",
                  path     = tempdir(), mute_citation = TRUE
              ),
              format = "fasta")

              out2 <- read_rna(getRNA(
                  db       = "refseq",
                  organism = "GCF_000146045.2",
                  path     = tempdir(), mute_citation = TRUE
              ),
              format = "fasta")

              expect_equal(out1, out2)
          })


test_that(
    "The getRNA() interface to NCBI Genbank works properly (including when command is repeated)..",
    {
        skip_on_cran()
        skip_on_travis()
        # test proper download from genbank
        out1 <- read_rna(
            getRNA(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

        # test proper use of internal referece files when command is repeated
        out2 <- read_rna(
            getRNA(
                db       = "genbank",
                organism = "Saccharomyces cerevisiae",
                path     = tempdir(), mute_citation = TRUE
            ),
            format = "fasta"
        )

        expect_equal(out1, out2)
    }
)

test_that("The getRNA() interface to NCBI Genbank works properly using taxids (including when command is repeated)..",
          {
              skip_on_cran()
              skip_on_travis()
              # test proper download from genbank
              out1 <- read_rna(getRNA(
                  db       = "genbank",
                  organism = "559292",
                  path     = tempdir(), mute_citation = TRUE
              ),
              format = "fasta")

              out2 <- read_rna(getRNA(
                  db       = "genbank",
                  organism = "559292",
                  path     = tempdir(), mute_citation = TRUE
              ),
              format = "fasta")

              expect_equal(out1, out2)
          })



test_that("The getRNA() interface to NCBI Genbank works properly using accession ids (including when command is repeated)..",
          {
              skip_on_cran()
              skip_on_travis()
              # test proper download from genbank
              out1 <- read_rna(getRNA(
                  db       = "genbank",
                  organism = "GCA_000146045.2",
                  path     = tempdir(), mute_citation = TRUE
              ),
              format = "fasta")

              out2 <- read_rna(getRNA(
                  db       = "genbank",
                  organism = "GCA_000146045.2",
                  path     = tempdir(), mute_citation = TRUE
              ),
              format = "fasta")

              expect_equal(out1, out2)
          })


test_that("The getRNA() interface to Ensembl works properly (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from ensemblgenomes
    out1 <- read_rna(
        getRNA(
            db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_rna(
        getRNA(
            db       = "ensembl",
            organism = "Saccharomyces cerevisiae",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    expect_equal(out1, out2)
})


test_that("The getRNA() interface to Ensembl works properly using taxid (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from ensemblgenomes
    out1 <- read_rna(
        getRNA(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_rna(
        getRNA(
            db       = "ensembl",
            organism = "4932",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    expect_equal(out1, out2)
})


test_that("The getRNA() interface to Ensembl works properly using accession id (including when command is repeated)..", {
    skip_on_cran()
    skip_on_travis()
    # test proper download from ensemblgenomes
    out1 <- read_rna(
        getRNA(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    out2 <- read_rna(
        getRNA(
            db       = "ensembl",
            organism = "GCA_000146045.2",
            path     = tempdir(), mute_citation = TRUE
        ),
        format = "fasta"
    )

    expect_equal(out1, out2)
})

